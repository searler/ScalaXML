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

 def pExtract = elem("strings", 
        elem(TURI,"str", attr("kind", text)))(TURI)

  def cd = elem(TURI, "str" ,elem(TURI, "c", text) ~ elem(TURI, "d", text))
  def ab =  elem(TURI, "str" ,elem(TURI, "a", text) ~ elem(TURI, "b", text))

def pPicklePartial:PartialFunction[String~String,XmlOutputStore=>XmlOutputStore] =  {
           case v => cd.pickle(v,_)
         }
def pUnpicklePartial:PartialFunction[String, St =>PicklerResult[String~String]] = {
           case "cd" => cd.unpickle
           case "ab" => ab.unpickle
           
         }  
 def pSwitch = elem("strings",
         switch(elem(TURI,"str", attr("kind", text)), pUnpicklePartial, pPicklePartial)
         )(TURI)

  

  def pSeq2 = 
    elem(TURI, "pair", 
        elem(TURI, "a", text) ~ elem(TURI, "b", text))
def pXml = 
    xml(TURI, "pair")

 def pListSeq2 = 
    elem(TURI, "pair", 
        list(elem(TURI, "a", text) ~ elem(TURI, "b", text)))

def pSetSeq2 = 
    elem(TURI, "pair", 
        set(elem(TURI, "a", text) ~ elem(TURI, "b", text)))

 def pMapSeq2 = 
    elem(TURI, "pair", 
        map(keyOnly(elem(TURI, "a", text),elem(TURI, "a", text) ~ elem(TURI, "b", text))))

 def pSeq2Start : Pickler[String ~ String] = 
    elem(TURI, "pair", 
        ignore(TURI,"x") ~> elem(TURI, "a", text)   ~ elem(TURI, "b", text)) 

  def pSeq2Skip: Pickler[String ~ String] = 
    elem(TURI, "pair", 
        (elem(TURI, "a", text) <~ ignore(TURI,"x")) ~ elem(TURI, "b", text) )

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


val inExtract =
      """<strings xmlns="testing-uri">
<str kind="special"/>
</strings>
"""    

  val inputSpecial =
      """<p:strings xmlns:p="testing-uri">
<p:st>one</p:st>
<p:str kind="special">this is special</p:str>
<p:a>a</p:a>
<p:b>b</p:b>
</p:strings>
"""    
          
  val input =
    """<pair xmlns="testing-uri">
<a>alfa</a>
<b>omega</b>
</pair>
"""

 val inputEmpty =
    """<pair xmlns="testing-uri">
<a>alfa</a>
<b></b>
</pair>
"""

val inputMultiple =
    """<pair xmlns="testing-uri">
<a>alfa</a>
<b>omega</b>
<a>delta</a>
<b>gamma</b>
</pair>
"""

val inputDuplicate =
    """<pair xmlns="testing-uri">
<a>alfa</a>
<b>omega</b>
<a>delta</a>
<b>gamma</b>
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
  val pairEmpty = new ~("alfa", "")
  val pairString: ~[String,String] = new ~("alfa", "omega")
  val pairInt = new ~("alfa", 12)
  val pairFloat = new ~("alfa", 12.34F)
  val pairOptNone = new ~("alfa",None)
  val pairOptSome = new ~("alfa",Some("omega"))
  val pairNestedSeq = (new ~("alfa", "omega")) ~ (new ~("beta", "phi"))
  val pairListSingle = List(new ~("alfa", "omega"))
  val pairSetSingle = Set(new ~("alfa", "omega"))
  val pairMapSingle = Map("alfa"->new ~("alfa", "omega"))
  val pairListTwo = List(new ~("alfa", "omega"), new ~("delta","gamma"))
  val pairMapTwo = Map("alfa"->new ~("alfa", "omega"),"delta"->new ~("delta", "gamma"))
  val pairSetTwo = Set(new ~("alfa", "omega"), new ~("delta","gamma"))
  
 


 "testAttrTURIUnpickle" in {
     assertSucceedsWith("Attribute unpickling failed", pair, attrInputTURI , pAttr2TURI)
  } 

"testAttrTURIPickle" in {
    val pickled = pAttr2TURI.pickle(pair)
    normalize(attrInputTURI) must beEqualTo(normalize(pickled))
  } 

 "testAttrConvert" in {
    val unpickled = pSeq2.unpickle(input).get
    val pickled = pAttr2TURI.pickle(unpickled)
    normalize(attrInputTURI) must beEqualTo(normalize(pickled))
  } 

 "testAttrUnpickle" in {
     assertSucceedsWith("Attribute unpickling failed", pair, attrInput , pAttr2)
  } 

"testAttrPickle" in {
    val pickled = pAttr2.pickle(pair)
    normalize(attrInput) must beEqualTo(normalize(pickled))
  } 

 "testSequencePickle" in  {
 val pickled = pSeq2.pickle(pair)
       normalize(input) must beEqualTo(normalize(pickled))
 }

 "testListSequencePickle" in  {
 val pickled = pListSeq2.pickle(pairListSingle)
       normalize(input) must beEqualTo(normalize(pickled))
 }

"testListTwoSequencePickle" in  {
 val pickled = pListSeq2.pickle(pairListTwo)
       normalize(inputMultiple) must beEqualTo(normalize(pickled))
 }

"testSetTwoSequencePickle" in  {
 val pickled = pSetSeq2.pickle(pairSetTwo)
       normalize(inputMultiple) must beEqualTo(normalize(pickled))
 }


 "testMapSequencePickle" in  {
 val pickled = pMapSeq2.pickle(pairMapSingle)
       normalize(input) must beEqualTo(normalize(pickled))
 }

"testMapTwoSequencePickle" in  {
 val pickled = pMapSeq2.pickle(pairMapTwo)
       normalize(inputMultiple) must beEqualTo(normalize(pickled))
 }

"testSequenceStartPickle" in  {
 val pickled = pSeq2Start.pickle(pair)
       normalize(input) must beEqualTo(normalize(pickled))
 }

"testSequenceSkipPickle" in  {
 val pickled = pSeq2Skip.pickle(pair)
       normalize(input) must beEqualTo(normalize(pickled))
 }

"testSequenceSkipPickle" in  {
 val pickled = pSeq2Start.pickle(pair)
       normalize(input) must beEqualTo(normalize(pickled))
 }



 "testNestedPickle" in  {
 val pickled = pNested2.pickle(pair)
       normalize(inputNested) must beEqualTo(normalize(pickled))
 }

"testNestedSeqPickle" in  {
 val pickled = pNestedSeq2.pickle(pairNestedSeq)
       normalize(inputNestedSeq) must beEqualTo(normalize(pickled))
 }

"testSequenceOptSomePickle" in  {
 val pickled = pSeq2Opt.pickle(pairOptSome)
       normalize(input) must beEqualTo(normalize(pickled))
 }

"testSequenceOptNonePickle" in  {
 val pickled = pSeq2Opt.pickle(pairOptNone)
       normalize(inputOpt) must beEqualTo(normalize(pickled))
 }

"testSequenceUnpickle" in  {
    assertSucceedsWith("Sequence unpickling failed", pair, input, pSeq2)
  }

"testSequenceEmptyUnpickle" in  {
    assertSucceedsWith("Sequence unpickling failed", pairEmpty, inputEmpty, pSeq2)
  }

"testXml" in  {
    
    val result = pXml.unpickle(LinearStore("""<pair xmlns="testing-uri">
<a>alfa</a>
<b>omega</b>
</pair>
"""))
    result match {
      case Success(v:org.w3c.dom.Element, _) =>  "\nalfa\nomega\n" must beEqualTo( v.getTextContent)
      case f: NoSuccess  =>  fail(f.toString)
    }
     val pickled = pXml.pickle(result.get)
       normalize(input) must beEqualTo(normalize(pickled))
  }

"testListSequenceUnpickle" in  {
    assertSucceedsWith("Sequence unpickling failed", pairListSingle, input, pListSeq2)
  }

"testSetSequenceUnpickle" in  {
    assertSucceedsWith("Sequence unpickling failed", pairSetSingle, input, pSetSeq2)
  }

"testMapSequenceUnpickle" in  {
    assertSucceedsWith("Sequence unpickling failed", pairMapSingle, input, pMapSeq2)
  }

"testListTwoSequenceUnpickle" in  {
    assertSucceedsWith("Sequence unpickling failed", pairListTwo, inputMultiple, pListSeq2)
  }

"testSetTwoUnpickle" in  {
    assertSucceedsWith("Sequence unpickling failed", pairSetTwo, inputMultiple, pSetSeq2)
  }

"testSetTwoDuplicateUnpickle" in  {
    assertSucceedsWith("Sequence unpickling failed", pairSetTwo, inputDuplicate, pSetSeq2)
  }

"testMapTwoSequenceUnpickle" in  {
    assertSucceedsWith("Sequence unpickling failed", pairMapTwo, inputMultiple, pMapSeq2)
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
       val pickled = pSeq2Int.pickle(pairInt)
       normalize(inputInt) must beEqualTo(normalize(pickled))
 }

 "testSequenceIntTypePickle" in  {
       val pickled = pSeq2IntType.pickle(pairInt)
       normalize(inputInt) must beEqualTo(normalize(pickled))
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

  def pStrings = elem(TURI, "strings", list(elem(TURI, "str", text)))
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
    val pickled = pStrings.pickle(strings)
    normalize(inputRep) must beEqualTo( normalize(pickled))
  }

  "testRepetition1Pickle" in {
    val inputRep = 
      """<strings xmlns="testing-uri">
<str>one</str>
</strings>
"""
      
    val strings = List("one")
    val pickled = pStrings.pickle(strings)
    normalize(inputRep) must beEqualTo( normalize(pickled))
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
    val pickled = pStrings.pickle(strings)
    normalize(inputRep)  must beEqualTo( normalize(pickled))
  }

"testExtractUnPickle" in {
    assertSucceedsWith("Unpickling when", "special", inExtract, pExtract)
  }

"testExtractPickle" in  {
    val pickled = pExtract.pickle("special")
    normalize(inExtract) must beEqualTo( normalize(pickled))
  }


   "testSwitchAB" in {
    val in =
      """<p:strings xmlns:p="testing-uri">
<p:str kind="ab">
<p:a>a</p:a>
<p:b>b</p:b>
</p:str>
</p:strings>
"""    
    
    val expected = new ~("a", "b") 
    assertSucceedsWith("Unpickling when", expected, in, pSwitch)
  }

"testSwitchCD" in {
    val in =
      """<p:strings xmlns:p="testing-uri">
<p:str kind="cd">
<p:c>c</p:c>
<p:d>d</p:d>
</p:str>
</p:strings>
"""    
    
    val expected = new ~("c", "d") 
    assertSucceedsWith("Unpickling when", expected, in, pSwitch)
  }

 
  

}

