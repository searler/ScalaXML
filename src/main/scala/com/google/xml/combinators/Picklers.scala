/* Copyright (c) 2008 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */


package com.google.xml.combinators

import com.google.gdata.data.util.DateTime

import java.text.ParseException
import org.w3c.dom._

/** 
 * A class for XML Pickling combinators.
 * <p>
 * A pickler for some type A is a class that can save objects of type A to XML (pickle)
 * and read XML back to objects of type A (unpickle). This class provides some basic 
 * building blocks (like text), and several combinators (like elem, attr, 
 * seq) to build more complex picklers.
 * <p>
 * Example:
 * <xmp>
 *   def picklePair: Pickler[String ~ String] = 
 *      elem("p", URI, "pair", 
 *         elem("p", URI, "a", text) ~ elem("p", URI, "b", text))
 *       
 *   val input =
 *     <p:pair xmlns:p="testing-uri">
 *       <p:a>alfa</p:a>
 *       <p:b>omega</p:b>
 *     </p:pair>
 * </xmp>
 * picklePair will be able to pickle and unpickle pairs of Strings that look like the
 * input.
 *
 * @author Iulian Dragos (iuliandragos@google.com)
 * @see <a href="http://www.fh-wedel.de/~si/HXmlToolbox/">Haskell XML Toolbox</a>, 
 * @see Andrew Kennedy's
 *      <a href="http://research.microsoft.com/%7Eakenn/fun/">Pickler Combinators</a>, 
 * @see <a 
 *   href="http://www.scala-lang.org/docu/files/api/scala/util/parsing/combinator/Parsers.html">
 * Scala combinator parsers</a>
 */
object Picklers extends AnyRef with TupleToPairFunctions {

  /**
   * The state of the pickler is a collection of attributes, a list of 
   * nodes (which might be Text nodes), and namespace bindings.
   */
  type St = XmlInputStore

  def emptyStore: XmlOutputStore = PlainOutputStore.empty

  /**
   * A class representing pickling results. It encapsulate the result and the state of 
   * the pickler. It can be either @see Success or @see Failure.
   */
  sealed abstract class PicklerResult[+A] {
    /** Apply 'f' when this result is successful. */
    def andThen[B](f: (A, St) => PicklerResult[B]): PicklerResult[B]
    
    /** Apply 'f' when this result is failure. */
    def orElse[B >: A](f: => PicklerResult[B]): PicklerResult[B]
    
    /** Is this result a successful parse result? */
    def isSuccessful: Boolean
    
    /** Retrieve the result in case of success. */
    def get: A
  }
  
  /** A successful parse.  */
  case class Success[+A](v: A, in: St) extends PicklerResult[A] {
    def andThen[B](f: (A, St) => PicklerResult[B]): PicklerResult[B] = f(v, in)
    def orElse[B >: A](f: => PicklerResult[B]): PicklerResult[B] = this
    def isSuccessful = true
    def get = v
  }

  /**
   * Parsing failed. There are two possible subclasses, Failure and Error. Failure is
   * recoverable, while Error makes the whole parsing fail. To make a pickler return 
   * errors, apply 'commit' to it. All 'elem' picklers will commit on their contents,
   * so that an error parsing the contents of an element will stop everything. This
   * is almost always the best thing to do (and yields the best error messages).
   */
  abstract class NoSuccess(val msg: String, val in: St) extends PicklerResult[Nothing] {
    def andThen[B](f: (Nothing, St) => PicklerResult[B]) = this
    def orElse[B >: Nothing](f: => PicklerResult[B]): PicklerResult[B] = f
    def isSuccessful = false
    def get = throw new NoSuchElementException("Unpickling failed.")

    val prefix: String

    override def toString = prefix + msg + " with input: " + in
  }
  
  /** A Failure means the parsing has failed, but alternatives can still be tried. */
  case class Failure(m: String, i: St) extends NoSuccess(m, i) {
    override val prefix = "Failure: " 
  }

  /** An Error is a failure which causes the entire parsing to fail (no alternatives are tried). */
  case class Error(m: String, i: St) extends NoSuccess(m, i) {
    override val prefix = "Error: "
  }
  
  /** Pickler for type A */
  abstract class Pickler[A] {
    def pickle(v: A, in: XmlOutputStore): XmlOutputStore
    def unpickle(in: St): PicklerResult[A]
    
    /** Sequential composition. This pickler will accept an A and then a B. */
    def ~[B](pb: => Pickler[B]): Pickler[~[A, B]] = 
      seq(this, pb)
  }

  /** A basic pickler that serializes a value to a string and back.  */
  def text: Pickler[String] = new Pickler[String] {
    def pickle(v: String, in: XmlOutputStore): XmlOutputStore = 
      in.addText(v)

    def unpickle(in: St): PicklerResult[String] = {
      in.acceptText match {
        case (Some(content), in1) => Success(content.getTextContent, in1)
        case (None, in1)                => Failure("Text node expected", in1)
      }
    }
  }

  /** A basic pickler that serializes an integer value to a string and back. */
  def intVal: Pickler[Int] = {
    def parseInt(literal: String, in: St): PicklerResult[Int] = try {
      Success(literal.toInt, in)
    } catch {
      case e: NumberFormatException => Failure("Integer literal expected", in) 
    }
    filter(text, parseInt, String.valueOf(_))
  }
  
  /**
   * A basic pickler for boolean values. Everything equal to the string 'true' is
   * unpickled to the boolean value <code>true</code>, everything else to <code>false</code>.
   * It is not case sensitive.
   */
  def boolVal: Pickler[Boolean] =
    wrap (text) (java.lang.Boolean.valueOf(_).booleanValue) (String.valueOf(_))
  
  /**
   * A basic pickler for floating point values. It accepts double values as specified by the
   * Scala and Java language. 
   * 
   * @see java.lang.Double.valueOf for the exact grammar.
   */
  def doubleVal: Pickler[Double] = {
    def parseDouble(literal: String, in: St): PicklerResult[Double] = try {
      Success(literal.toDouble, in)
    } catch {
      case e: NumberFormatException => Failure("Floating point literal expected", in)
    }
    
    filter(text, parseDouble, String.valueOf(_))
  }
  
  

  /**
   * A pickler for date/time in RFC 3339 format. It handles dates that look like
   * <code>2008-02-15T16:16:02+01:00</code>. The time offset can be replaced by Z 
   * (zulu time) when it is zero (UTC time).
   * 
   * @param allowDateOnly When true, accepts date component alone.
   * @see http://atomenabled.org/developers/syndication/atom-format-spec.php#date.constructs
   */
  def dateTime(allowDateOnly: Boolean): Pickler[DateTime] = new Pickler[DateTime] {
    def pickle(v: DateTime, in: XmlOutputStore): XmlOutputStore = 
      in.addText(v.toString)
      
    def unpickle(in:St): PicklerResult[DateTime] = 
      in.acceptText match {
        case (Some(n:Node), in1) =>
          try {
            val str = n.getTextContent
            if (allowDateOnly)
              Success(DateTime.parseDateOrDateTime(str), in1)
            else
              Success(DateTime.parseDateTime(str), in1)
          } catch {
            case e: ParseException => Failure("Invalid date: " + e.getMessage, in1)
          }
        case (None, in1) => 
          Failure("Expected date in textual format", in1) 
      }
  }
  
  /**
   * A pickler for date/time in RFC 3339 format. It handles dates that look like
   * <code>2008-02-15T16:16:02+01:00</code>. The time offset can be replaced by Z 
   * (zulu time) when it is zero (UTC time). The time component is required.
   * 
   * @see http://atomenabled.org/developers/syndication/atom-format-spec.php#date.constructs
   */
  def dateTime: Pickler[DateTime] = dateTime(false)
  
  /** 
   * Apply a pair of functions on the result of pa. Unlike 'wrap', 'f' may cause the 
   * pickler to fail. 
   * 
   * For an example, see the implementation of intVal. 
   */
  def filter[A, B](pa: => Pickler[A], f: (A, St) => PicklerResult[B], g: B => A): Pickler[B] =
    new Pickler[B] {
      def pickle(v: B, in: XmlOutputStore): XmlOutputStore =
        pa.pickle(g(v), in)
      def unpickle(in: St): PicklerResult[B] = 
        pa.unpickle(in) andThen { (v, in1) => f(v, in) } 
    }
  
  /**
   * A constant pickler: it always pickles 'v'. Unpickle fails when the value that is found 
   * is not equal to 'v'. 
   */
  def const[A](pa: => Pickler[A], v: A): Pickler[A] = new Pickler[A] {
    def pickle(ignored: A, in: XmlOutputStore) = pa.pickle(v, in)
    def unpickle(in: St): PicklerResult[A] = {
      pa.unpickle(in) andThen {(v1, in1) => 
        if (v == v1) 
          Success(v1, in1) 
        else 
          Failure("Expected '" + v + "', but " + v1 + " found.", in1)
      }
    }
  }

  /** A pickler for default values. If 'pa' fails, returns 'v' instead. */
  def default[A](pa: => Pickler[A], v: A): Pickler[A] =  
    wrap (opt(pa)) ({ 
      case Some(v1) => v1
      case None => v
  }) (v => Some(v))

  /** A marker pickler: 'true' when the unpickler succeeds, false otherwise. */
  def marker(pa: => Pickler[String]): Pickler[Boolean] = 
    wrap (opt(pa)) { 
      case Some(_) => true
      case None => false
    } (b => if (b) Some("") else None)
  
  /** Convenience method for creating an attribute within a namepace. */
  def attr[A](label: String, pa: => Pickler[A], ns: (String, String)): Pickler[A] =
    attr(ns._1, ns._2, label, pa)

  /**
   * Wrap a parser into a prefixed attribute. The attribute will contain all the 
   * content produced by 'pa' in the 'nodes' field.
   */
  def attr[A](pre: String, uri: String, key: String, pa: => Pickler[A]) = new Pickler[A] {
    def pickle(v: A, in: XmlOutputStore) = {
      in.addAttribute(pre,uri, key, v.toString)
    }

    def unpickle(in: St): PicklerResult[A] = {
      in.acceptAttr(key, uri) match {
        case (Some(node), in1) =>
          pa.unpickle(LinearStore(NullNamedNodeMap, List(node))) andThen { (v, in2) => Success(v, in1) }
        case (None, in1) => 
          Failure("Expected attribute " + pre + ":" + key + " in " + uri, in)
      }
    }
  }
  
  /**
   * A pickler for unprefixed attributes. Such attributes have no namespace.
   */
  def attr[A](label: String, pa: => Pickler[A]): Pickler[A] = new Pickler[A] {
    def pickle(v: A, in: XmlOutputStore) = {
       in.addAttribute(label, v.toString)
    }
      
    def unpickle(in: St): PicklerResult[A] = {
      in.acceptAttr(label) match {
        case (Some(node), in1) =>
          pa.unpickle(LinearStore(NullNamedNodeMap, List(node))) andThen { (v, in2) => Success(v, in1) }
        case (None, in1) => 
          Failure("Expected unprefixed attribute " + label, in)
      }
    }
  }
  
  /** 
   * Convenience method for creating an element with an implicit namepace. Contents of
   * this element are committed (this parser is not allowed to recover from failures in
   * parsing its content.
   */
  def elem[A](label: String, pa: => Pickler[A])(implicit ns: (String, String)): Pickler[A] =
    elem(ns._1, ns._2, label, commit(pa))

  /** Wrap a pickler into an element. */
  def elem[A](pre: String, uri: String, label: String, pa: => Pickler[A]) = new Pickler[A] {
    def pickle(v: A, in: XmlOutputStore): XmlOutputStore = {
      pa.pickle(v, in.addNode(pre, uri,label))
      in
    }

    def unpickle(in: St): PicklerResult[A] = {
      in.acceptElem(label, uri) match {
        case (Some(e: Element), in1) => 
          pa.unpickle(LinearStore.enterElem(e)) andThen { (v, in2) =>
            Success(v, in1)
          }
          
        case _ => 
          Failure("Expected a <" + pre + ":" + label + "> in " + uri, in)
      }
    }
  }

  /** Sequential composition of two picklers */
  def seq[A, B](pa: => Pickler[A], pb: => Pickler[B]): Pickler[~[A, B]] =  new Pickler[~[A, B]] {
    def pickle(v: A ~ B, in: XmlOutputStore): XmlOutputStore = 
      pb.pickle(v._2, pa.pickle(v._1, in))
    
    def unpickle(in: St): PicklerResult[~[A, B]] = {
      pa.unpickle(in) match {
        case Success(va, in1) =>
          pb.unpickle(in1) match {
            case Success(vb, in2) => Success(new ~(va, vb), in2)
            case f: NoSuccess     => f
          }
        case f: NoSuccess => f
      }
    }
  }

  /** 
   * Convenience method for creating an element with interleaved elements. Elements enclosed
   * by the given element label can be parsed in any order. Any unknown elements are ignored.
   * <p/>
   * Example: 
   *   <code>interleaved("entry", elem("link", text) ~ elem("author", text))</code> will
   * will parse an element entry with two subelements, link and author, in any order, with
   * possibly other elements between them.
   */
  def interleaved[A](label: String, pa: => Pickler[A])(implicit ns: (String, String)): Pickler[A] =
    elem(label, interleaved(pa))(ns)

  /**
   * Transform the given parser into a parser that accepts permutations of its containing 
   * sequences. That is, interleaved(a ~ b ~ c) will parse a, b, c in any order (with possibly 
   * other elements in between. It should not be called directly, instead use the
   * interleaved which wraps an element around the interleaved elements.  
   */
  def interleaved[A](pa: => Pickler[A]): Pickler[A] = new Pickler[A] {
    def pickle(v: A, in: XmlOutputStore): XmlOutputStore = pa.pickle(v, in)
    
    def unpickle(in: St): PicklerResult[A] = 
      pa.unpickle(in.randomAccessMode) andThen { (v, in1) => 
        Success(v, in1.linearAccessMode)
      }
  }
   
  /** 
   * A commit parser. Failures are transformed to errors, so alternatives (when combined with 
   * other parsers) are not tried. 
   */
  def commit[A](pa: => Pickler[A]): Pickler[A] = new Pickler[A] {
    def pickle(v: A, in: XmlOutputStore): XmlOutputStore = pa.pickle(v, in)
    def unpickle(in: St): PicklerResult[A] = pa.unpickle(in) match {
      case s: Success[_] => s
      case Failure(msg, in1) => Error(msg, in1)
      case e: Error => e
    }
  }

  /**
   * Return a pickler that always pickles the first value, but unpickles using the second when the
   * first one fails.
   */
  def or[A](pa: => Pickler[A], qa: => Pickler[A]): Pickler[A] = new Pickler[A] {
    def pickle(v: A, in: XmlOutputStore): XmlOutputStore = 
      pa.pickle(v, in)
      
    def unpickle(in: St): PicklerResult[A] = 
      pa.unpickle(in) match {
        case s: Success[_] => s
        case f: Failure => qa.unpickle(in)
        case e: Error => e
      }
  }
  
  /**
   * An optional pickler. It pickles v when it is there, and leaves the input unchanged when empty.
   * It unpickles the value when the underlying parser succeeds, and returns None otherwise.
   */
  def opt[A](pa: => Pickler[A]) = new Pickler[Option[A]] {
    def pickle(v: Option[A], in: XmlOutputStore) = v match {
      case Some(v) => pa.pickle(v, in)
      case None    => in
    }
    
    def unpickle(in: St): PicklerResult[Option[A]] = 
      pa.unpickle(in) andThen {(v, in1) => Success(Some(v), in1) } orElse Success(None, in)
  }
  
  /** A repetition pickler. It applies 'pa' until there it fails. */
  def rep[A](pa: => Pickler[A]): Pickler[List[A]] = new Pickler[List[A]] {
    def pickle(vs: List[A], in: XmlOutputStore): XmlOutputStore = vs match {
      case v :: vs => pickle(vs, pa.pickle(v, in))
      case Nil     => in
    }
    
    def unpickle(in: St): PicklerResult[List[A]] = { 
      val res1 = pa.unpickle(in).andThen { (v: A, in1: St) => 
         val Success(vs, in2) = unpickle(in1)
         Success(v :: vs, in2)
      } 
      res1 match {
        case s: Success[_] => s
        case f: Failure => Success(Nil, in)
        case e: Error => e
      }
    }
  }
  
  /**
   * Runs 'pb' unpickler on the first element that 'pa' successfully parses. It
   * is more general than 'interleaved', which uses only the element name to decide 
   * the input on which to run a pickler. 'pa' can be arbitrarily complex.
   * 
   * Example:
   *   when(elem("feedLink", const(attr("rel", "#kinds"), rel)), kindsPickler)
   * 
   * will look for the first 'feedLink' element with an attribute equal to '#kinds'
   * and then run 'kindsPickler' on that element.
   */
  def when[A, B](pa: => Pickler[A], pb: => Pickler[B]): Pickler[B] = new Pickler[B] {
    def pickle(v: B, in: XmlOutputStore) = pb.pickle(v, in)
    
    def unpickle(in: St) = {
      var lastFailed: Option[NoSuccess] = None
      
      val target = in.nodes find {
        case e: Element => 
          pa.unpickle(LinearStore.fromElem(e)) match {
            case _: Success[_] => true
            case f: NoSuccess => lastFailed = Some(f); false 
          }
        case _ => false
      }
      
      target match {
        case Some(e: Element) => 
          pb.unpickle(LinearStore.fromElem(e)) match {
            case Success(v1, in1) =>
              Success(v1, in.mkState(in.attrs, in.nodes.toList.filterNot(_ == e)))
            case f: NoSuccess =>
              Failure(f.msg, in)
          }
        case None => 
          if (lastFailed.isDefined)
            lastFailed.get
          else
            Failure("Expected at least one element", in)
      }
    }
  }

  /** Wrap a pair of functions around a given pickler */
  def wrap[A, B](pb: => Pickler[B])(g: B => A)(f: A => B): Pickler[A] = new Pickler[A] {
    def pickle(v: A, in: XmlOutputStore): XmlOutputStore = 
      pb.pickle(f(v), in)

    def unpickle(in: St): PicklerResult[A] = 
      pb.unpickle(in) match {
        case Success(vb, in1) => Success(g(vb), in1)
        case f: NoSuccess     => f
      }
  }


  def wrapCaseClass[A, B](pa: => Pickler[A])(f: A => B)(g: B => Some[A]): Pickler[B] =
    wrap(pa) (f) { x => g(x).get }

  
  
  
  
  
  /** A logging combinator */
  def logged[A](name: String, pa: => Pickler[A]): Pickler[A] = new Pickler[A] {
    def pickle(v: A, in: XmlOutputStore): XmlOutputStore = {
      println("pickling [" + name + "] " + v + " at: " + in)
      val res = pa.pickle(v, in)
      println("got back: " + res)
      res
    }

    def unpickle(in: St) = {
      println("unpickling " + name + " at: " + in)
      val res = pa.unpickle(in)
      println("got back: " + res)
      res
    }
  }
}

/** Convenience class to hold two values (it has lighter syntax than pairs). */
final case class ~[+A, +B](_1: A, _2: B) {
  override def toString = "~(" + _1 + ", " + _2 + ")"
  
  /** Append another value to this pair. */
  def ~[C](c: C) = new ~(this, c)
}
