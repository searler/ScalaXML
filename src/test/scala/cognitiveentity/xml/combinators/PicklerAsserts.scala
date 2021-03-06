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

package cognitiveentity.xml.combinators;

import _root_.org.specs2.mutable._

import org.w3c.dom._

/**
 * This trait defines specialized asserts for testing picklers.
 *
 * @author Iulian Dragos (iuliandragos@google.com)
 */
trait PicklerAsserts extends Specification {
  import Picklers._
   
  def assertSucceedsWith[A](name: String, expected: A, in: String, pa: Pickler[A]) = {
    val result = pa.unpickle(in)
    result match {
      case Success(v, _) =>  expected aka name must beEqualTo( v)
      case f: NoSuccess  =>  failure(f.toString)
    }
  }
  
  /** Test that the value 'v' pickles to the expected xml node. */
  def assertPicklesTo[A](name: String, expected: Node, v: A, pa: Pickler[A]) = {
     val doc = pa.pickleDocument(v)
     expected aka name must beEqualTo(doc)
  }
  
  /**
   * Return a string representation without unnecessary white-space.
   * Useful when comparing XML documents.
   */
  def normalize(n: Node): String = {
     val writer = new java.io.StringWriter
     val transformer = javax.xml.transform.TransformerFactory.newInstance().newTransformer(); 
     transformer.setOutputProperty(javax.xml.transform.OutputKeys.OMIT_XML_DECLARATION,"yes")
     transformer.setOutputProperty(javax.xml.transform.OutputKeys.INDENT,"yes")
     transformer.transform(new javax.xml.transform.dom.DOMSource(n),new javax.xml.transform.stream.StreamResult(writer))
     writer.toString
  }

 def normalize(xml:XmlOutputStore):String = normalize(xml.document) 

  def normalize(s:String) = s
}
