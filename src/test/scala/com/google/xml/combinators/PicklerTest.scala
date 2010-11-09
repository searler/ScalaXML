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

object PicklerTest extends  PicklerAsserts{
  import Picklers._
  
  final val TURI = URI("testing-uri")

  def pAttr2 = 
    elem(TURI, "pair" , attr("a",text) ~ attr("b",text) )

  def pAttr2TURI = 
    elem(TURI, "pair" , attr(TURI,"a",text) ~ attr(TURI,"b",text) )
       
  def pNested2: Pickler[String ~ String] = 
    elem(TURI, "pair", 
            elem(TURI,"n",
                elem(TURI, "a", text) ~ elem(TURI, "b", text)))   

 def pNestedSeq2: Pickler[(String ~ String) ~ (String ~ String)] = 
    elem(TURI, "pair", 
            elem(TURI,"n",
                elem(TURI, "a", text) ~ elem(TURI, "b", text)) ~ 
            elem(TURI,"o",
                elem(TURI, "c", text) ~ elem(TURI, "d", text))
    )    

  def pSeq2 = 
    elem(TURI, "pair", 
        elem(TURI, "a", text) ~ elem(TURI, "b", text))

 def pSeq2Start = 
    elem(TURI, "pair", 
        ignore(TURI,"x") <~ elem(TURI, "a", text)   ~ elem(TURI, "b", text)) 

  def pSeq2Skip = 
    elem(TURI, "pair", 
        elem(TURI, "a", text) ~> ignore(TURI,"x") ~ elem(TURI, "b", text) )

  def pSeq2Int: Pickler[String ~ Int] = 
    elem(TURI, "pair", 
        elem(TURI, "a", text) ~ elem(TURI, "b", intVal))

 import com.google.xml.combinators.Converters._
 def pSeq2IntType: Pickler[String ~ Int] = 
    elem(TURI, "pair", 
        elem(TURI, "a", text) ~ elem(TURI, "b", typedValue))

 def pSeq2FloatType: Pickler[String ~ Float] = 
    elem(TURI, "pair", 
        elem(TURI, "a", text) ~ elem(TURI, "b", typedValue))

 def pSeq2StringType: Pickler[String ~ String] = 
    elem(TURI, "pair", 
        elem(TURI, "a", text) ~ elem(TURI, "b", typedValue))

 def pSeq2Opt: Pickler[String ~Option[String]] = 
    elem(TURI, "pair", 
        elem(TURI, "a", text)  ~ opt(elem(TURI,"b",text)))

 def pSeq2Default: Pickler[String ~ String] = 
    elem(TURI, "pair", 
        elem(TURI, "a", text)  ~ default(elem(TURI,"b",text),"omega"))
          
  val input =
    """<pair xmlns="testing-uri">
<a>alfa</a>
<b>omega</b>
</pair>
"""

 val inputSkip =
    """<pair xmlns="testing-uri">
<a>alfa</a>
<x>xxx</x>
<b>omega</b>
</pair>
"""

val inputStart =
    """<pair xmlns="testing-uri">
<x>xxx</x>
<a>alfa</a>
<b>omega</b>
</pair>
"""

val inputNested =
    """<pair xmlns="testing-uri">
<n>
<a>alfa</a>
<b>omega</b>
</n>
</pair>
"""

val inputNestedSeq =
    """<pair xmlns="testing-uri">
<n>
<a>alfa</a>
<b>omega</b>
</n>
<o>
<c>beta</c>
<d>phi</d>
</o>
</pair>
"""


val inputInt =
    """<pair xmlns="testing-uri">
<a>alfa</a>
<b>12</b>
</pair>
"""



val inputFloat =
    """<pair xmlns="testing-uri">
<a>alfa</a>
<b>12.34</b>
</pair>
"""
val inputOpt =
    """<pair xmlns="testing-uri">
<a>alfa</a>
</pair>
"""

  val attrInput =
    """<pair a="alfa" b="omega" xmlns="testing-uri"/>
"""

val attrInputTURI =
    """<pair xmlns:ns0="testing-uri" ns0:a="alfa" xmlns:ns1="testing-uri" ns1:b="omega" xmlns="testing-uri"/>
"""
       

  val pair = new ~("alfa", "omega")
  val pairString: ~[String,String] = new ~("alfa", "omega")
  val pairInt = new ~("alfa", 12)
  val pairFloat = new ~("alfa", 12.34F)
  val pairOptNone = new ~("alfa",None)
  val pairOptSome = new ~("alfa",Some("omega"))
  val pairNestedSeq = (new ~("alfa", "omega")) ~ (new ~("beta", "phi"))

 


 "testAttrTURIUnpickle" in {
     assertSucceedsWith("Attribute unpickling failed", pair, attrInputTURI , pAttr2TURI)
  } 

"testAttrTURIPickle" in {
    val pickled = pAttr2TURI.pickle(pair, PlainOutputStore.empty)
    normalize(attrInputTURI) must beEqualTo(normalize(pickled.document))
  } 

 "testAttrConvert" in {
    val unpickled = pSeq2.unpickle(LinearStore.fromString(input)).get
    val pickled = pAttr2TURI.pickle(unpickled, PlainOutputStore.empty)
    normalize(attrInputTURI) must beEqualTo(normalize(pickled.document))
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

"testSequenceStartPickle" in  {
 val pickled = pSeq2Start.pickle(pair, PlainOutputStore.empty)
       normalize(input) must beEqualTo(normalize(pickled.document))
 }

"testSequenceSkipPickle" in  {
 val pickled = pSeq2Skip.pickle(pair, PlainOutputStore.empty)
       normalize(input) must beEqualTo(normalize(pickled.document))
 }

"testSequenceSkipPickle" in  {
 val pickled = pSeq2Start.pickle(pair, PlainOutputStore.empty)
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

"testSequenceSkipUnpickle" in  {
    assertSucceedsWith("Sequence unpickling failed", pair, inputSkip, pSeq2Skip)
  }

"testSequenceStartUnpickle" in  {
    assertSucceedsWith("Sequence unpickling failed", pair, inputStart, pSeq2Start)
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

  "testSequenceIntTypeUnpickle" in  {
    assertSucceedsWith("Sequence unpickling failed", pairInt, inputInt, pSeq2IntType)
  }

 "testSequenceFloatTypeUnpickle" in  {
    assertSucceedsWith("Sequence unpickling failed", pairFloat, inputFloat, pSeq2FloatType)
  }

"testSequenceStringTypeUnpickle" in  {
    assertSucceedsWith("Sequence unpickling failed", pair, input, pSeq2StringType)
  }

"testSequenceStringTypeUnpickleInferred" in  {
    assertSucceedsWith("Sequence unpickling failed", pairString, input, pSeq2StringType)
  }

 "testSequenceIntPickle" in  {
       val pickled = pSeq2Int.pickle(pairInt, PlainOutputStore.empty)
       normalize(inputInt) must beEqualTo(normalize(pickled.document))
 }

 "testSequenceIntTypePickle" in  {
       val pickled = pSeq2IntType.pickle(pairInt, PlainOutputStore.empty)
       normalize(inputInt) must beEqualTo(normalize(pickled.document))
 }
  
  def pSeq3: Pickler[String ~ String ~ String] =
    elem(TURI, "triple",
        elem(TURI, "a", text)
      ~ elem(TURI, "b", text)
      ~ elem(TURI, "c", text))
      
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

  def pStrings = elem(TURI, "strings", rep(elem(TURI, "str", text)))
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
    val inputRep = """<strings xmlns="testing-uri"/>
"""
      
    val strings = List()
    val pickled = pStrings.pickle(strings, PlainOutputStore.empty)
    normalize(inputRep) must beEqualTo( normalize(pickled.document))
  }

  "testRepetition1Pickle" in {
    val inputRep = 
      """<strings xmlns="testing-uri">
<str>one</str>
</strings>
"""
      
    val strings = List("one")
    val pickled = pStrings.pickle(strings, PlainOutputStore.empty)
    normalize(inputRep) must beEqualTo( normalize(pickled.document))
  }

  "testRepetition3Pickle" in {
    val inputRep = 
      """<strings xmlns="testing-uri">
<str>one</str>
<str>two</str>
<str>three</str>
</strings>
"""
      
    val strings = List("one", "two", "three")
    val pickled = pStrings.pickle(strings, PlainOutputStore.empty)
    normalize(inputRep)  must beEqualTo( normalize(pickled.document))
  }
  
  "testWhen" in {
    implicit val ns =  URI("testing-uri")
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
    implicit val ns = URI("testing-uri")
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

