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

import java.io.{Reader,  StringReader}
import org.xml.sax.InputSource

import scala.io.Source


import org.w3c.dom._

import javax.xml.parsers._


/**
 * This class encapsulate the state carried around
 * when pickling or unpickling XML. This is an immutable data structure.
 * Speaking from the point of view of unpickling, the store consists of a
 * set of attributes not yet consumed, a set of nodes not yet consumed and
 * a set of namespace bindings encountered so far.
 *
 * @author Iulian Dragos (iuliandragos@google.com)
 */
class LinearStore(ats: NamedNodeMap, nods: List[Node]) 
    extends XmlInputStore {
  def attrs = ats
  def nodes = nods
 
  
 
  
  /**
   * Accept the given element, or fail. Succeeds when the given element is the head of the node
   * list. Comments, processing instructions and white space are skipped if 'skipsWhitespace' is
   * set (default). 
   */
  def acceptElem(label: String, uri:URI): (Option[Node], XmlInputStore) = {
    if (nodes.isEmpty)
      (None, this)
    else 
      nodes.head match {
        case e:Element  if (e.getNamespaceURI ==  uri.uri && e.getLocalName == label) => 
          (Some(e), mkState(attrs, nodes.tail))
        case _ => (None, this)
      }
  }
  
  /**
   * Accept the given prefixed attribute, or fail. Succeeds when the given attribute exists
   * (order does not matter). Returns a Seq[Node], since attributes may contain text nodes 
   * interspersed with entity references.
   */
  def acceptAttr(label: String, uri:URI): (Option[Node], XmlInputStore) = {
      attrs.getNamedItemNS(uri.uri, label) match {
        case null  => (None, this)
        case contents =>
          (Some(contents), this)
      }
  }

  /**
   * Accept the given unprefixed attribute, or fail. Succeeds when the given attribute exists
   * (order does not matter). Returns a Seq[Node], since attributes may contain text nodes 
   * interspersed with entity references.
   */
  def acceptAttr(label: String): (Option[Node], XmlInputStore) = {
      attrs.getNamedItem(label) match {
        case null  => (None, this)
        case contents =>
          (Some(contents), this)
      }
  }
  
  /** Accept a text node. Fails if the head of the node list is not a text node. */
  def acceptText: (Option[Node], XmlInputStore) = {
    if (nodes.isEmpty) 
     (None, this)// (Some(Text("")), this) ### not None??
    else 
        (Some(nodes.head), mkState(attrs, nodes.tail))
      /*nodes.head match {
        case t: Text => (Some(t.getTextContent), mkState(attrs, nodes.tail))
        case _       => (None, this)
      }*/
  }

  protected def mkState(attrs: NamedNodeMap, nodes: Seq[Node], level: Int) = 
    LinearStore(attrs, nodes)
  
  override def toString = 
    "LinearStore(" + attrs + ", " + nodes.mkString("", ",", "") + ", " 
  
 
}

/**
 * Convenience object for creating LinearStores
 *
 * @author Iulian Dragos
 */
object LinearStore {
 
  
  /** Return an empty pickler state  */
  def empty() = 
    LinearStore(NullNamedNodeMap, Nil)

  /** Create a LinearStore with the given state.*/
  def apply(attrs: NamedNodeMap, nodes: Seq[Node]) = 
    new LinearStore(attrs, nodes.toList )
  
  def apply(store: XmlStore): XmlInputStore =
    apply(store.attrs, store.nodes)

  /** Create a LinearStore from an element. */
  def fromElem(e: Element) =
    LinearStore(e.getAttributes, List(e))
  
  /** Create a LinearStore from the given Reader. */
  def fromReader(in: Reader) = {
    val factory = DocumentBuilderFactory.newInstance
    factory setNamespaceAware true
    factory setIgnoringComments true
    val builder = factory.newDocumentBuilder
    fromElem(builder.parse(new InputSource(in)).getDocumentElement)
  }
  
  /** Create a LinearStore from the string. */
  def fromString(f: String) = {
    fromReader(new StringReader(f))
  }

  /** Create a LinearStore for the contents of the given element. */ 
  def enterElem(e: Element) = 
    LinearStore(e.getAttributes, makeList(e.getChildNodes))

  private def makeList(l:NodeList):List[Node] = {
    val buffer = new scala.collection.mutable.ListBuffer[Node]
    for(i<-0 until l.getLength){
      val n = l.item(i)
      n match {
         case t:Text if t.getTextContent.trim.isEmpty => 
         case p:ProcessingInstruction => 
         case c:Comment=> //none expected
         case _ =>  buffer += n
      }
    }
    buffer.toList
  }

  
}
