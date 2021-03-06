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

import scala.collection._

import org.w3c.dom._

import javax.xml.parsers.{DocumentBuilderFactory,DocumentBuilder}

/**
 * An XML store used during pickling. It provides methods for adding
 * XML elements, attributes and namespaces. Implementers decide on the
 * actual strategy for looking up elements based on name (linear or 
 * random access).
 * 
 * @author Iulian Dragos
 * @author Richard Searle
 */
trait XmlOutputStore{

  /** Return a new XmlStore with a new attribute prepended to the list of attrs */
  def addAttribute(uri:URI, key: String, value: String): XmlOutputStore
  
  /** Return a new XmlStore with an unprefixed attribute appended to the list of attrs. */
  def addAttribute(key: String, value: String): XmlOutputStore
   
  /** Add a text node */
  def addText(s: String): XmlOutputStore 

  
  /** Add a node. */
  def addNode(uri:URI, label: String) = {
    val e = document.createElementNS(uri.uri, label)
    addElement(e)
    new ElementOutputStore(e)
  }

  def addElement(e:Element)
  def importElement(e:Element)

  def document:Document
}


class ElementOutputStore(val element:Element) extends XmlOutputStore {

  def document =element  getOwnerDocument
  

/** Add a text node */
  def addText(s: String): XmlOutputStore = {
    element.setTextContent(s)
    this
  }
  
  /** Return a new LinearStore with a prefixed attribute prepended to the list of attrs */
  def addAttribute(uri:URI,key: String, value: String): XmlOutputStore = {
    element.setAttributeNS(uri.uri,key,value)
    this
  }

  /** Return a new LinearStore with an unprefixed attribute prepended to the list of attrs */
  def addAttribute(key: String, value: String): XmlOutputStore = {
    element.setAttribute(key,value)
    this
  }
    
  def addElement(e:Element) {
    element.appendChild(e)
  }

  def importElement(e:Element) {
    element.appendChild(document.importNode(e,true))
  }
   
}

class DocumentOutputStore(val document:Document) extends XmlOutputStore {
  def addText(s: String): XmlOutputStore =  throw new UnsupportedOperationException
  def addAttribute(uri:URI,key: String, value: String): XmlOutputStore =  throw new UnsupportedOperationException
  def addAttribute(key: String, value: String): XmlOutputStore =  throw new UnsupportedOperationException

  def addElement(e:Element) {
    document.appendChild(e)
  }

 def  importElement(e:Element) {
    document.appendChild(document.importNode(e,true))
  }
 
}

/** Factory for output stores. */
object PlainOutputStore {

  private val builder = DocumentBuilderFactory.newInstance.newDocumentBuilder 
  /** An empty output store. */
  def empty: XmlOutputStore = new DocumentOutputStore(builder.newDocument) 
}

