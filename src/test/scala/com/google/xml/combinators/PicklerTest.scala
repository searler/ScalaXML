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

/*
  Extracted from gdata-scala
  Modified by Richard Searle
*/

   
package com.google.xml.combinators

import org.specs._



/** 
 * This class tests simple XML pickler combinators.
 *
 * @author Iulian Dragos (iuliandragos@google.com) 
 */

class PicklerTest extends  PicklerAsserts{
  import Picklers._
  
  final val URI = "testing-uri"

  def pAttr2 = 
    elem("p", URI, "pair" , attr("a",text) ~ attr("b",text) )

  def pAttr2URI = 
    elem("p", URI, "pair" , attr("p", URI,"a",text) ~ attr("p", URI,"b",text) )
       
  def pNested2: Pickler[String ~ String] = 
    elem("p", URI, "pair", 
            elem("p",URI,"n",
                elem("p", URI, "a", text) ~ elem("p", URI, "b", text)))   

 def pNestedSeq2: Pickler[(String ~ String) ~ (String ~ String)] = 
    elem("p", URI, "pair", 
            elem("p",URI,"n",
                elem("p", URI, "a", text) ~ elem("p", URI, "b", text)) ~ 
            elem("p",URI,"o",
                elem("p", URI, "c", text) ~ elem("p", URI, "d", text))
    )    

  def pSeq2: Pickler[String ~ String] = 
    elem("p", URI, "pair", 
        elem("p", URI, "a", text) ~ elem("p", URI, "b", text))

  def pSeq2Int: Pickler[String ~ Int] = 
    elem("p", URI, "pair", 
        elem("p", URI, "a", text) ~ elem("p", URI, "b", intVal))

 def pSeq2Opt: Pickler[String ~Option[String]] = 
    elem("p", URI, "pair", 
        elem("p", URI, "a", text)  ~ opt(elem("p",URI,"b",text)))

 def pSeq2Default: Pickler[String ~ String] = 
    elem("p", URI, "pair", 
        elem("p", URI, "a", text)  ~ default(elem("p",URI,"b",text),"omega"))
          
  val input =
    """<p:pair xmlns:p="testing-uri">
<p:a>alfa</p:a>
<p:b>omega</p:b>
</p:pair>
"""

val inputNested =
    """<p:pair xmlns:p="testing-uri">
<p:n>
<p:a>alfa</p:a>
<p:b>omega</p:b>
</p:n>
</p:pair>
"""

val inputNestedSeq =
    """<p:pair xmlns:p="testing-uri">
<p:n>
<p:a>alfa</p:a>
<p:b>omega</p:b>
</p:n>
<p:o>
<p:c>beta</p:c>
<p:d>phi</p:d>
</p:o>
</p:pair>
"""


val inputInt =
    """<p:pair xmlns:p="testing-uri">
<p:a>alfa</p:a>
<p:b>12</p:b>
</p:pair>
"""
val inputOpt =
    """<p:pair xmlns:p="testing-uri">
<p:a>alfa</p:a>
</p:pair>
"""

  val attrInput =
    """<p:pair a="alfa" b="omega" xmlns:p="testing-uri"/>
"""

val attrInputURI =
    """<p:pair xmlns:p="testing-uri" p:a="alfa" p:b="omega"/>
"""
       

  val pair = new ~("alfa", "omega")
  val pairInt = new ~("alfa", 12)
  val pairOptNone = new ~("alfa",None)
  val pairOptSome = new ~("alfa",Some("omega"))
  val pairNestedSeq = (new ~("alfa", "omega")) ~ (new ~("beta", "phi"))

 


 "testAttrURIUnpickle" in {
     assertSucceedsWith("Attribute unpickling failed", pair, attrInputURI , pAttr2URI)
  } 

"testAttrURIPickle" in {
    val pickled = pAttr2URI.pickle(pair, PlainOutputStore.empty)
    normalize(attrInputURI) must beEqualTo(normalize(pickled.document))
  } 

 "testAttrConvert" in {
    val unpickled = pSeq2.unpickle(LinearStore.fromString(input)).get
    val pickled = pAttr2URI.pickle(unpickled, PlainOutputStore.empty)
    normalize(attrInputURI) must beEqualTo(normalize(pickled.document))
  } 

 "testAttrUnpickle" in {
     assertSucceedsWith("Attribute unpickling failed", pair, attrInput , pAttr2)
  } 

"testAttrPickle" in {
    val pickled = pAttr2.pickle(pair, PlainOutputStore.empty)
    normalize(attrInput) must beEqualTo(normalize(pickled.document))
  } 

 "testSequencePickle" in  {
 val pickled = pSeq2.pickle(pair, PlainOutputStore.empty)
       normalize(input) must beEqualTo(normalize(pickled.document))
 }

 "testNestedPickle" in  {
 val pickled = pNested2.pickle(pair, PlainOutputStore.empty)
       normalize(inputNested) must beEqualTo(normalize(pickled.document))
 }

"testNestedSeqPickle" in  {
 val pickled = pNestedSeq2.pickle(pairNestedSeq, PlainOutputStore.empty)
       normalize(inputNestedSeq) must beEqualTo(normalize(pickled.document))
 }

"testSequenceOptSomePickle" in  {
 val pickled = pSeq2Opt.pickle(pairOptSome, PlainOutputStore.empty)
       normalize(input) must beEqualTo(normalize(pickled.document))
 }

"testSequenceOptNonePickle" in  {
 val pickled = pSeq2Opt.pickle(pairOptNone, PlainOutputStore.empty)
       normalize(inputOpt) must beEqualTo(normalize(pickled.document))
 }

"testSequenceUnpickle" in  {
    assertSucceedsWith("Sequence unpickling failed", pair, input, pSeq2)
  }

"testNestedUnpickle" in  {
    assertSucceedsWith("Sequence unpickling failed", pair, inputNested, pNested2)
  }

"testNestedSeqUnpickle" in  {
    assertSucceedsWith("Sequence unpickling failed", pairNestedSeq, inputNestedSeq, pNestedSeq2)
  }

"testSequenceUnpickleOptSome" in  {
    assertSucceedsWith("Sequence unpickling failed", pairOptSome, input, pSeq2Opt)
  }

"testSequenceUnpickleOptNone" in  {
    assertSucceedsWith("Sequence unpickling failed", pairOptNone, inputOpt, pSeq2Opt)
  }

"testSequenceUnpickleDefaultNone" in  {
    assertSucceedsWith("Sequence unpickling failed", pair, inputOpt, pSeq2Default)
  }
  
  "testSequenceIntUnpickle" in  {
    assertSucceedsWith("Sequence unpickling failed", pairInt, inputInt, pSeq2Int)
  }
  
  def pSeq3: Pickler[String ~ String ~ String] =
    elem("p", URI, "triple",
        elem("p", URI, "a", text)
      ~ elem("p", URI, "b", text)
      ~ elem("p", URI, "c", text))
      
  val triple = (new ~("alfa", "beta")) ~ "gamma" 
  val inputTriple =
    """<m:triple xmlns:m="testing-uri">
       <m:a>alfa</m:a>
       <m:b>beta</m:b>
       <m:c>gamma</m:c>
     </m:triple>"""
  
  "testSequence3Unpickle" in  {
    assertSucceedsWith("Sequence 3 unpickling failed", triple, inputTriple, pSeq3)
  }

  def pStrings = elem("p", URI, "strings", rep(elem("p", URI, "str", text)))
  "testRepetition0Unpickle" in  {
    val inputRep = """<p:strings xmlns:p="testing-uri"></p:strings>"""
      
    val strings = List()
    assertSucceedsWith("Repetition with empty sequence failed",
        strings, inputRep, pStrings)
  }

  "testRepetition1Unpickle" in  {
    val inputRep = 
      """<p:strings xmlns:p="testing-uri">
         <p:str>one</p:str>
       </p:strings>"""
      
    val strings = List("one")
    assertSucceedsWith("Repetition with one element failed",
        strings, inputRep, pStrings)
  }
  
  "testRepetition3Unpickle" in  {
    val inputRep = 
      """<p:strings xmlns:p="testing-uri">
         <p:str>one</p:str>
         <p:str>two</p:str>
         <p:str>three</p:str>
       </p:strings>"""

    val strings = List("one", "two", "three")
    assertSucceedsWith("Repetition failed", strings, inputRep, pStrings)
  }
  
  "testRepetition0Pickle" in  {
    val inputRep = """<p:strings xmlns:p="testing-uri"/>
"""
      
    val strings = List()
    val pickled = pStrings.pickle(strings, PlainOutputStore.empty)
    normalize(inputRep) must beEqualTo( normalize(pickled.document))
  }

  "testRepetition1Pickle" in {
    val inputRep = 
      """<p:strings xmlns:p="testing-uri">
<p:str>one</p:str>
</p:strings>
"""
      
    val strings = List("one")
    val pickled = pStrings.pickle(strings, PlainOutputStore.empty)
    normalize(inputRep) must beEqualTo( normalize(pickled.document))
  }

  "testRepetition3Pickle" in {
    val inputRep = 
      """<p:strings xmlns:p="testing-uri">
<p:str>one</p:str>
<p:str>two</p:str>
<p:str>three</p:str>
</p:strings>
"""
      
    val strings = List("one", "two", "three")
    val pickled = pStrings.pickle(strings, PlainOutputStore.empty)
    normalize(inputRep)  must beEqualTo( normalize(pickled.document))
  }
  
  "testWhen" in {
    implicit val ns = ("p", "testing-uri")
    val input =
      """<p:strings xmlns:p="testing-uri">
<p:str>one</p:str>
<p:str kind="special">this is special</p:str>
<p:a>a</p:a>
<p:b>b</p:b>
</p:strings>
"""
    val pickler = elem("strings", 
      (when(elem("str", const(attr("kind", text), "special")), elem("str", text))
       ~ elem("str", text) ~ elem("a", text) ~ elem("b", text)))
      
    val expected = new ~("this is special", "one") ~ "a" ~ "b"
    assertSucceedsWith("Unpickling when", expected, input, pickler)
  }
  
  "testWhenInterleaved" in {
    implicit val ns = ("p", "testing-uri")
    val input =
      """<p:strings xmlns:p="testing-uri">
<p:b>b</p:b>
<p:a>a</p:a>
<p:str>one</p:str>
<p:str kind="special">this is special</p:str>
</p:strings>
"""
    val pickler = interleaved("strings", 
      (when(elem("str", const(attr("kind", text), "special")), elem("str", text))
       ~ elem("str", text) ~ elem("a", text) ~ elem("b", text)))
      
    val expected = new ~("this is special", "one") ~ "a" ~ "b"
    assertSucceedsWith("Unpickling when", expected, input, pickler)
  }

}

