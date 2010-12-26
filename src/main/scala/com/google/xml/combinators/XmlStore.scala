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


package com.google.xml.combinators

import java.io.{InputStream, InputStreamReader}


import scala.collection.mutable.{Buffer, ListBuffer}

import scala.xml.NamespaceBinding

import org.w3c.dom._

/**
 * An interface for XML stores. It keeps around a collection of attributes, elements and 
 * namespace bindings.
 * 
 * @see com.google.xml.combinators.XmlInputStore
 * @see com.google.xml.combinators.XmlOutputStore
 * @author Iulian Dragos
 * @author Richard Searle
 */
trait XmlStore {
  /** The current XML attributes. */
  def attrs: NamedNodeMap

  /** The current XML nodes. */
  def nodes: Seq[Node]
 

}
