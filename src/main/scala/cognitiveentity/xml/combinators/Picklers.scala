/* Copyright (c) 2008 Google Inc.
 * Copyright (c) 2010 Richard Searle
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

/*
  Extracted from gdata-scala
  Modified by Richard Searle
*/

package cognitiveentity.xml.combinators

import java.text.ParseException
import org.w3c.dom._
import cognitiveentity.xml.combinators.Converters._

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
 * @author Richard Searle
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
    
    /** Retrieve the result in case of success. */
    def get: A
  }
  
  /** A successful parse.  */
  case class Success[+A](v: A, in: St) extends PicklerResult[A] {
    def andThen[B](f: (A, St) => PicklerResult[B]): PicklerResult[B] = f(v, in)
    def orElse[B >: A](f: => PicklerResult[B]): PicklerResult[B] = this
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

   def |(pb: => Pickler[A]): Pickler[A] = 
      or(this, pb)

    def <~[B](pb: => Pickler[B]): Pickler[A] = 
      dropRight(this, pb)

    def ~>[B](pb: => Pickler[B]): Pickler[B] = 
      dropLeft(this, pb)

    def pickle(v: A): XmlOutputStore = pickle(v,PlainOutputStore.empty)
    def pickleDocument(v:A) :Document = pickle(v).document

    def unpickle(s:String): PicklerResult[A] = unpickle(LinearStore(s))
    def unpickle(doc:Document): PicklerResult[A] = unpickle(LinearStore(doc))
    def unpickle(element:Element): PicklerResult[A] = unpickle(LinearStore(element))


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
           pa.unpickle(LinearStore(e)) match {
             case _: Success[_] => true
             case f: NoSuccess => lastFailed = Some(f); false 
           }
         case _ => false
       }
       
       target match {
         case Some(e: Element) => 
           pb.unpickle(LinearStore(e)) match {
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
 

  /** A basic pickler that serializes a value to a string and back.  */
  def text: Pickler[String] = new Pickler[String] {
    def pickle(v: String, in: XmlOutputStore): XmlOutputStore = 
      in.addText(v)

    def unpickle(in: St): PicklerResult[String] = {
      in.acceptText match {
        case (Some(content), in1) => Success(content, in1)
        case (None, in1)          => Success("", in1)
      }
    }
  }

  def typedValue[T](implicit tc:Convert[T]): Pickler[T] = {
    def parse(literal: String, in: St): PicklerResult[T] = try {
      Success(tc.parse(literal), in)
    } catch {
      case _ => Failure("Cannot parse", in) 
    }
    filter(text, parse, tc.unparse(_))
  }

  /** A basic pickler that serializes an integer value to a string and back. */
  def intVal = typedValue[Int]
  
  /**
   * A basic pickler for boolean values. Everything equal to the string 'true' is
   * unpickled to the boolean value <code>true</code>, everything else to <code>false</code>.
   * It is not case sensitive.
   */
  def boolVal = typedValue[Boolean]
  
  /**
   * A basic pickler for floating point values. It accepts double values as specified by the
   * Scala and Java language. 
   * 
   * @see java.lang.Double.valueOf for the exact grammar.
   */
  def doubleVal = typedValue[Double]
  
  

  
  
  
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
 

  /**
   * Wrap a parser into a prefixed attribute. The attribute will contain all the 
   * content produced by 'pa' in the 'nodes' field.
   */
  def attr[A](uri:URI, key: String, pa: => Pickler[A]) = new Pickler[A] {
    def pickle(v: A, in: XmlOutputStore) = {
      in.addAttribute(uri, key, v.toString)
    }

    def unpickle(in: St): PicklerResult[A] = {
      in.acceptAttr(key, uri) match {
        case (Some(node), in1) =>
          pa.unpickle(LinearStore(List(node))) andThen { (v, in2) => Success(v, in1) }
        case (None, in1) => 
          Failure("Expected attribute " +  key + " in " + uri, in)
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
          pa.unpickle(LinearStore( List(node))) andThen { (v, in2) => Success(v, in1) }
        case (None, in1) => 
          Failure("Expected unprefixed attribute " + label, in)
      }
    }
  }


  /**
   * Silently consumes the element when unpickling
   * and no-op when pickling.
   */
   def ignore(uri:URI, label: String) = new Pickler[Unit] {
    def pickle(v: Unit, in: XmlOutputStore): XmlOutputStore = {
      in
    }

    def unpickle(in: St): PicklerResult[Unit] = {
      in.acceptElem(label, uri) match {
        case (Some(e: Element), in1) =>   Success((), in1)
        case _ => 
          Failure("Expected a <" + label + "> in " + uri, in)
      }
    }
  }


  /**
   * Pickle an DOM subtree as an Element.
   * Can be used to provide extensibility at the cost of dealing
   * with the standard Java DOM.
   */
  def xml(uri:URI, label: String) = new Pickler[org.w3c.dom.Element] {
    def pickle(v: org.w3c.dom.Element, in: XmlOutputStore): XmlOutputStore = {
      in.importElement(v)
      in
    }

    def unpickle(in: St): PicklerResult[org.w3c.dom.Element] = {
      in.acceptElem(label, uri) match {
        case (Some(v: Element), in1) =>  Success(v, in1)
        case _ => 
          Failure("Expected a <" + label + "> in " + uri, in)
      }
    }
  }

  
  /** 
   * Convenience method for creating an element with an implicit namepace. Contents of
   * this element are committed (this parser is not allowed to recover from failures in
   * parsing its content.
   */
  def elem[A](label: String, pa: => Pickler[A])(implicit uri:URI): Pickler[A] =
    elem(uri, label, commit(pa))
 
  def elem[A](uri:URI, label: String)(implicit tc:Convert[A]):Pickler[A] =
    elem(uri,label,typedValue(tc))
 
   def elem[A](label: String)(implicit uri:URI,tc:Convert[A]):Pickler[A] =
    elem(uri,label,typedValue(tc))



  /** Wrap a pickler into an element. */
  def elem[A](uri:URI, label: String, pa: => Pickler[A]) = new Pickler[A] {
    def pickle(v: A, in: XmlOutputStore): XmlOutputStore = {
      pa.pickle(v, in.addNode(uri,label))
      in
    }

    def unpickle(in: St): PicklerResult[A] = {
      in.acceptElem(label, uri) match {
        case (Some(e: Element), in1) => 
          pa.unpickle(LinearStore.enterElem(e)) andThen { (v, in2) =>
            Success(v, in1)
          }
          
        case _ => 
          Failure("Expected a <" + label + "> in " + uri, in)
      }
    }
  }

 def dropRight[A, B](pa: => Pickler[A], pb: => Pickler[B]): Pickler[A] =  new Pickler[A] {
    def pickle(v: A , in: XmlOutputStore): XmlOutputStore = 
      pa.pickle(v, in)
    
    def unpickle(in: St): PicklerResult[A] = {
      pa.unpickle(in) match {
        case Success(va, in1) =>
          pb.unpickle(in1) match {
            case Success(vb, in2) => Success(va, in2)
            case f: NoSuccess     => f
          }
        case f: NoSuccess => f
      }
    }
  }

  def dropLeft[A, B](pa: => Pickler[A], pb: => Pickler[B]): Pickler[B] =  new Pickler[B] {
    def pickle(v: B , in: XmlOutputStore): XmlOutputStore = 
      pb.pickle(v, in)
    
    def unpickle(in: St): PicklerResult[B] = {
      pa.unpickle(in) match {
        case Success(va, in1) =>
          pb.unpickle(in1) match {
            case Success(vb, in2) => Success(vb, in2)
            case f: NoSuccess     => f
          }
        case f: NoSuccess => f
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
  def interleaved[A](label: String, pa: => Pickler[A])(implicit uri:URI): Pickler[A] =
    elem(label, interleaved(pa))(uri)

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
  
  /** A repetition pickler, returning a List of matched values. It applies 'pa' until there it fails. */
  def list[A](pa: => Pickler[A]): Pickler[List[A]] = new Pickler[List[A]] {
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
  * A repetition pickler, returning a Set of matched values.
  */
 def set[A](pa: => Pickler[A]): Pickler[Set[A]] = new Pickler[Set[A]] {
    def pickle(vs: Set[A], in: XmlOutputStore): XmlOutputStore = {
      if(vs.isEmpty)
         in
      else {
        val v:A = vs.head
        pickle(vs-v, pa.pickle(v, in))
       }
    }
    
    def unpickle(in: St): PicklerResult[Set[A]] = { 
      val res1 = pa.unpickle(in).andThen { (v: A, in1: St) => 
         val Success(vs, in2) = unpickle(in1)
         Success(vs+v, in2)
      } 
      res1 match {
        case s: Success[_] => s
        case f: Failure => Success(Set(), in)
        case e: Error => e
      }
    }
  }

  /**
   * Apply pb iff pa succeeds, with the same input, returning a tuple2.
   * This pickler is used for creating maps where the key must also form part of the value.
   * It is thus necessary to unpickle twice, once to get the key and once to get the same data
   * for the value. This is inefficient if same unpickler is used in each case, which would
   * generally be the case.
   * Only pb is used when pickling. i.e. pb must be a subset of pa.
   */
  def twice[A,B](pa: => Pickler[A], pb: => Pickler[B]):Pickler[(A,B)] = new Pickler[(A,B)]{
    def pickle(v: (A,B),in:XmlOutputStore): XmlOutputStore = {
       pb.pickle(v._2,in)
    }
    def unpickle(in: St):PicklerResult[(A,B)] = {
      pa.unpickle(in) match {
         case Success(va,_) => {
           pb.unpickle(in) match {
              case Success(vb,in2) => Success((va,vb),in2)
              case f: NoSuccess => f
           }    
         }
         case f: NoSuccess => f
      } 
    }
  }

   /** Sequential composition of two picklers, returning a tuple2 from pa and pa~pb.
    *  Useful when creating a map with key->key+value.
    * Similar semantics to twice, but avoids the repeated unpickle of the key.
    */
  def join[A, B](pa: => Pickler[A], pb: => Pickler[B]): Pickler[(A,~[A, B])] =  new Pickler[(A,~[A, B])] {
    def pickle(v:(A, A ~ B), in: XmlOutputStore): XmlOutputStore = 
      pb.pickle(v._2._2, pa.pickle(v._1, in))
    
    def unpickle(in: St): PicklerResult[(A,~[A, B])] = {
      pa.unpickle(in) match {
        case Success(va, in1) =>
          pb.unpickle(in1) match {
            case Success(vb, in2) => Success(va -> new ~(va, vb), in2)
            case f: NoSuccess     => f
          }
        case f: NoSuccess => f
      }
    }
  }

  /**
   * Unpickle a map, given a pickler that returns a tuple2 of key and value.
   * The twice and join picklers can be used to create that tuple2.
   */
  def map[A,B](pa: => Pickler[(A,B)]): Pickler[Map[A,B]] = new Pickler[Map[A,B]] {
    def pickle(vs: Map[A,B], in: XmlOutputStore): XmlOutputStore = {
      if(vs.isEmpty)
         in
      else {
        val v:(A,B) = vs.head
        pickle(vs-v._1, pa.pickle(v, in))
       }
    }
    
    def unpickle(in: St): PicklerResult[Map[A,B]] = { 
      val res1 = pa.unpickle(in).andThen { (v: (A,B), in1: St) => 
         val Success(vs, in2) = unpickle(in1)
         Success(vs+v, in2)
      } 
      res1 match {
        case s: Success[_] => s
        case f: Failure => Success(Map(), in)
        case e: Error => e
      }
    }
  }

  def switch[A,B](pa: => Pickler[A],uf: => PartialFunction[A,St=>PicklerResult[B]], pf: => PartialFunction[B,XmlOutputStore=>XmlOutputStore]):Pickler[B] = new Pickler[B]{
     def pickle(v:B, in:XmlOutputStore) = pf(v)(in)
     def unpickle(in :St) : PicklerResult[B] = {
        pa.unpickle(in) match {
          case Success(v,_) => uf(v)(in)
          case f:NoSuccess => f
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
