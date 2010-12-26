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


package com.google.xml.combinators;

import org.specs._

/**
 * Test permutation parsers.
 *
 * @author Iulian Dragos (iuliandragos@google.com)
 */
class PicklerPermuteTest extends PicklerAsserts {
  import Picklers._
  
  final val TURI = URI("testing-uri")
  implicit val namespace = TURI
  
  def pPermute2: Pickler[String ~ String] =
    interleaved("set2", elem("a", text) ~ elem("b", text))
      
  val pair = new ~("alfa", "omega")
  val triple = new ~(new ~("alfa", "beta"), "gamma") 


  "testPermute2Unpickle" in {
    val perm1 = 
      """<p:set2 xmlns:p="testing-uri">
        <p:a>alfa</p:a>
        <p:b>omega</p:b>
      </p:set2>"""
    assertSucceedsWith("Permutation of 2 failed unpickling",
        pair, perm1, pPermute2)
  }
  
  "testPermute2Unpickle1" in {
    val perm1 = 
      """<p:set2 xmlns:p="testing-uri">
        <p:b>omega</p:b>
        <p:a>alfa</p:a>
      </p:set2>"""
    assertSucceedsWith("Permutation of 2 failed unpickling",
        pair, perm1, pPermute2)
  }

  def pPermute3: Pickler[String ~ String ~ String] =
    interleaved("set3", 
        elem("a", text)
      ~ elem("b", text)
      ~ elem("c", text))
            
  "testPermute3UnpickleAbc" in {
    val perm1 = 
      """<p:set3 xmlns:p="testing-uri">
        <p:a>alfa</p:a>
        <p:b>beta</p:b>
        <p:c>gamma</p:c>
      </p:set3>"""
    assertSucceedsWith("Permutation of 2 failed unpickling",
        triple, perm1, pPermute3)
  }

  "testPermute3UnpickleAcb" in {
    val perm1 = 
      """<p:set3 xmlns:p="testing-uri">
        <p:a>alfa</p:a>
        <p:c>gamma</p:c>
        <p:b>beta</p:b>
      </p:set3>"""
    assertSucceedsWith("Permutation of 3 failed unpickling",
        triple, perm1,  pPermute3)
  }

  "testPermute3UnpickleBac" in {
    val perm1 = 
      """<p:set3 xmlns:p="testing-uri">
        <p:b>beta</p:b>
        <p:a>alfa</p:a>
        <p:c>gamma</p:c>
      </p:set3>"""
    assertSucceedsWith("Permutation of 2 failed unpickling",
        triple, perm1,  pPermute3)
  }
  
  "testPermute3UnpickleBca" in {
    val perm1 = 
      """<p:set3 xmlns:p="testing-uri">
        <p:b>beta</p:b>
        <p:c>gamma</p:c>
        <p:a>alfa</p:a>
      </p:set3>"""
    assertSucceedsWith("Permutation of 2 failed unpickling",
        triple, perm1, pPermute3)
  }
  
  "testPermute3UnpickleCab" in {
    val perm1 = 
      """<p:set3 xmlns:p="testing-uri">
        <p:c>gamma</p:c>
        <p:a>alfa</p:a>
        <p:b>beta</p:b>
      </p:set3>"""
    assertSucceedsWith("Permutation of 2 failed unpickling",
        triple, perm1, pPermute3)
  }

  "testPermute3UnpickleCba" in {
    val perm1 = 
      """<p:set3 xmlns:p="testing-uri">
        <p:c>gamma</p:c>
        <p:b>beta</p:b>
        <p:a>alfa</p:a>
      </p:set3>"""
    assertSucceedsWith("Permutation of 2 failed unpickling",
        triple, perm1, pPermute3)
  }

  def pa: Pickler[String] = elem(TURI, "a", text)
  def pb: Pickler[String] = elem(TURI, "b", text)
  def pc: Pickler[String] = elem(TURI, "c", text)
  def pd: Pickler[String] = elem(TURI, "d", text)
  def pe: Pickler[String] = elem(TURI, "e", text)
  
  def pDPermuteE = elem(TURI, "elems", pd ~ interleaved("inner", pa ~ pb ~ pc) ~ pe)
  val objAbc = new ~(new ~("a", "b"), "c")
  val obj = new ~(new ~("d", objAbc), "e") 
  
  "testSeqPermutePickleUnpickle" in {
    val input = 
      """<elems xmlns="testing-uri">
<d>d</d>
<inner>
<a>a</a>
<b>b</b>
<c>c</c>
</inner>
<e>e</e>
</elems>
"""
      
    val pickled = pDPermuteE.pickle(obj, PlainOutputStore.empty).document
     normalize(input) must beEqualTo( normalize(pickled))
    assertSucceedsWith("Sequence and permutation failed unpickling",
        obj, input, pDPermuteE)
  }

  "testSeqPermuteUnpickleCab" in {
    val input = 
      """<p:elems xmlns:p="testing-uri">
        <p:d>d</p:d>
        <p:inner>
          <p:c>c</p:c>
          <p:a>a</p:a>
          <p:b>b</p:b>
        </p:inner>
        <p:e>e</p:e>
      </p:elems>"""
      
    assertSucceedsWith("Sequence and permutation failed unpickling",
        obj, input, pDPermuteE)
  }

  "testSeqPermuteUnpickleCabExtra" in {
    val input = 
      """<p:elems xmlns:p="testing-uri">
        <p:d>d</p:d>
        <p:inner>
          <p:c>c</p:c>
          <p:a>a</p:a>
          <p:b>b</p:b>
        </p:inner>
        <p:e>e</p:e>
        <extra>This tag should be ignored</extra>
      </p:elems>"""
      
    assertSucceedsWith("Sequence and permutation with unknown elements failed unpickling",
        obj, input, pDPermuteE)
  }
  
  "testNesting" in  {
    val p1 = interleaved("set3", interleaved(elem("a", text)) ~ elem("b", text) ~ elem("c", text))
    val input = 
      """<p:set3 xmlns:p="testing-uri">
        <p:a>alfa</p:a>
        <p:c>gamma</p:c>
        <p:b>beta</p:b>
      </p:set3>"""

    assertSucceedsWith("Nested interleaved", triple, input, p1)
  }

}
