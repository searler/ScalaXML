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


import org.w3c.dom._


/**
 * This class represents the input of picklers. It keeps around XML attributes,
 * nodes and current namespace bindings. There are two modes in which input stores
 * operate: linear and random access. A linear store will return elements in the 
 * order in which they are defined in the XML document. A random access store will
 * look the element up anywhere in the 'nodes' collection. Switching between the 
 * two modes is done by calling 'randomAccessMode' and 'linearAccessMode'. The modes
 * nest (randomAccessMode and linearAccessMode calls have to match). 
 * 
 * All 'accept' methods return an instance of XmlInputStore which is the input after
 * the accepted element was consumed. Implementers may choose whether to implement 
 * it using mutable state (and return 'this') or use an immutable representation.
 *
 * @see cognitiveentity.xml.combinators.LinearStore
 * @see cognitiveentity.xml.combinators.RandomAccessStore
 * @author Iulian Dragos
 * @author Richard Searle
 */
trait XmlInputStore extends XmlStore {
 

  /** 
   * The nesting level of randomAccessMode calls. The level is equal to the number of
   * times 'interleaved' has been called (and we have not yet left the interleaved mode).
   */
  protected[combinators] var randomAccessLevel = 0

  /**
   * Accept the given element, or fail. Succeeds when the given element is the head of the node
   * list. Comments, processing instructions and entity references might. 
   */
  def acceptElem(Label: String, uri:URI): (Option[Node], XmlInputStore)

  /**
   * Accept the given prefixed attribute, or fail. Succeeds when the given attribute exists
   * (order does not matter). Returns a Seq[Node], since attributes may contain text nodes 
   * interspersed with entity references.
   */
  def acceptAttr(label: String, uri:URI): (Option[Node], XmlInputStore)

  /**
   * Accept the given unprefixed attribute, or fail. Succeeds when the given attribute exists
   * (order does not matter). Returns a Seq[Node], since attributes may contain text nodes 
   * interspersed with entity references.
   */
  def acceptAttr(label: String): (Option[Node], XmlInputStore)
  
  /** Accept a text node. Fails if the head of the node list is not a text node. */
  def acceptText: (Option[String], XmlInputStore)

  /**
   * Enter random access mode. If the random access level is greater than 0, we try to reuse
   * the current type (my calling the factory method mkState). Otherwise, we create an 
   * instance of RandomAccessStore.
   */
  def randomAccessMode: XmlInputStore = {
    if (randomAccessLevel > 0)
      mkState(attrs, nodes,  randomAccessLevel + 1)
    else
      new RandomAccessStore(attrs, nodes,  1)
  }
  
  /**
   * Switch back to linear access mode. We take care of nesting, by switching to a linear
   * xml store only when the randomAccessLevel nesting is 0. This allows nesting of 'interleaved'
   * combinators.
   */
  def linearAccessMode: XmlInputStore = {
    if (randomAccessLevel == 0) 
      this
    else if (randomAccessLevel == 1)
      LinearStore(attrs, nodes)
    else {
      mkState(attrs, nodes,  randomAccessLevel - 1)
    }
  }
  
  /** Create a new xml store with the given data. Preserves the current randomAccessLevel */
  protected[combinators] def mkState(attrs: NamedNodeMap, nodes: Seq[Node]): XmlInputStore =
    mkState(attrs, nodes,  randomAccessLevel)
  
  /** Implementers should create a new instance of their class, given the Xml store data. */
  protected def mkState(attrs: NamedNodeMap, nodes: Seq[Node], 
     level: Int): XmlInputStore
}

