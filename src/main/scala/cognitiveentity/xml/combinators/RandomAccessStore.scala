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
 * This class matches elements at any position in the sequence of nodes. This allows
 * unpicklers to accept any permutation of a defined sequence. 
 * 
 * For efficiency reasons, this class uses a mutable representation for elements. When
 * an instance is created, this class creates a map from element labels (regardless of
 * namespace) to XML elements. This allows access in constant time implementation of
 * the 'acceptElem' method.
 * 
 * @see cognitiveentity.xml.combinators.XmlInputStore
 *
 * @author Richard Searle
 */
class RandomAccessStore(myAttrs: NamedNodeMap, myNodes: Seq[Node], 
    level: Int) extends LinearStore(myAttrs, myNodes.toList) {
  import collection.mutable.{Set, MultiMap}
  import collection.mutable.LinkedHashMap
  
  randomAccessLevel = level

  /** A cache of node, from element node to the Entries. */
  private val nodeMap = 
    new LinkedHashMap[String, Set[Entry]] with MultiMap[String, Entry]

  /** A holder class that provides proper identity to nodes. @see NodeBuffer.hashCode. */
  private class Entry(val n: Node)

  {
    // initialize store by mapping names to elements.
    for (n <- myNodes) 
      nodeMap.addBinding(n.getLocalName, new Entry(n))
  }

  def this(underlying: XmlInputStore) = 
    this(underlying.attrs, underlying.nodes,  underlying.randomAccessLevel)
  
  /**
   * Lookup the given element, based on label and URI. It uses the node map to efficiently 
   * perform lookups and removal.
   */
  override def acceptElem(label: String, uri:URI): (Option[Node], RandomAccessStore) = {
    for (elems <- nodeMap.get(label);
         entry <- elems)
      entry.n match {
        case e: Element if (e.getNamespaceURI == uri.uri) => 
          nodeMap.remove(label)
          return (Some(e), this)
        case _ => ()
      }
    (None, this)
  }
  
  /** Return the list of nodes. It reads them from the internal map. */
  override def nodes: List[Node] = {
    val buf = new scala.collection.mutable.ListBuffer[Node]
    for (ns <- nodeMap.values; entry <- ns.iterator) {
      buf += entry.n
    }
    buf.toList
  }
  
  /** Create a new instance of this class, given the contents of this store. */
  override protected[combinators] def mkState(attrs: NamedNodeMap, nodes: Seq[Node],
       level: Int) =
    new RandomAccessStore(attrs, nodes,  level)
    
  override def toString = "RandomAccessStore(" + attrs + ", " + 
    nodes.mkString("", ",", "") +  ")"
}


