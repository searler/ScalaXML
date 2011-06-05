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

//cannot be placed inside Specification, due to conflicts with Specs2 implicits
object definitions {

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

  // --- switch -- 
  /**
   * Unpickle a~b OR c~d, depending on the value of the kind attribute of the str element
   * 
   */
  def cd = elem(TURI, "str" ,elem(TURI, "c", intVal) ~ elem(TURI, "d", text))
  def ab =  elem(TURI, "str" ,elem(TURI, "a", text) ~ elem(TURI, "b", text))
  def pPicklePartial:PartialFunction[_~String,XmlOutputStore=>XmlOutputStore] =  {
           case cognitiveentity.xml.combinators.~(a:String,b:String) => ab.pickle(new ~(a,b),_)
           case cognitiveentity.xml.combinators.~(c:Int,d:String)  => cd.pickle(new ~(c,d),_)
           case v:cognitiveentity.xml.combinators.~[String,String] => ab.pickle(v,_)
           case v:cognitiveentity.xml.combinators.~[Int,String] => cd.pickle(v,_)
         }
  def pUnpicklePartial:PartialFunction[Any, St =>PicklerResult[Any~String]] = {
           case "cd" => cd.unpickle
           case "ab" => ab.unpickle         
         }  
 def pSwitch = elem("strings",
         switch(elem(TURI,"str", attr("kind", text)), pUnpicklePartial, pPicklePartial)
         )(TURI)
 // --- switch ---

   def pWhen = elem("strings", 
      ((when(elem(TURI,"str", const(attr("kind", text), "special")), elem(TURI,"str", text)) |
       when(elem(TURI,"str", const(attr("kind", text), "other")), elem(TURI,"str", text)))
       ~ elem(TURI,"st", text) ~ elem(TURI,"a", text) ~ elem(TURI,"b", text)))(TURI)

  def pPath = elem(TURI,"Profile", elem(TURI,"entry", elem(TURI,"value",text)))
  def pPathWhen = elem(TURI,"Profile", when(elem(TURI,"entry", const(elem(TURI,"kind",text),"b")),elem(TURI,"entry",ignore(TURI,"kind")~>elem(TURI,"value",text))))

   //only works because elem("a") is also a String
   def pMixed = 
    elem(TURI, "pair", text | elem(TURI, "a", text) )

  def pOptMixed = 
    elem(TURI, "pair", opt(text) ~  elem(TURI, "a", text))

  def pSeq2 = 
    elem(TURI, "pair", 
        elem(TURI, "a", text) ~ elem(TURI, "b", text))

 def pSeqMarker2 = 
    elem(TURI, "pair", 
        elem(TURI, "a", text) ~ marker(elem(TURI, "b", text)))

 def pSeq2Logged = 
    elem(TURI, "pair", 
        elem(TURI, "a", text) ~ logged("B",elem(TURI, "b", text)))

  def pSeqNoNS2 = 
    elem(NURI, "pair", 
        elem(NURI, "a", text) ~ elem(NURI, "b", text))

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
        map(twice(elem(TURI, "a", text),elem(TURI, "a", text) ~ elem(TURI, "b", text))))

  def pMapJoin2 = 
    elem(TURI, "pair", 
        map(join(elem(TURI, "a", text),elem(TURI, "b", text))))

 def pSeq2Start : Pickler[String ~ String] = 
    elem(TURI, "pair", 
        ignore(TURI,"x") ~> elem(TURI, "a", text)   ~ elem(TURI, "b", text)) 

  def pSeq2Skip: Pickler[String ~ String] = 
    elem(TURI, "pair", 
        (elem(TURI, "a", text) <~ ignore(TURI,"x")) ~ elem(TURI, "b", text) )

  def pSeq2Int: Pickler[String ~ Int] = 
    elem(TURI, "pair", 
        elem(TURI, "a", text) ~ elem(TURI, "b", intVal))

 import cognitiveentity.xml.combinators.Converters._
 def pSeq2IntType: Pickler[String ~ Int] = 
    elem(TURI, "pair", 
        elem(TURI, "a", text) ~ elem(TURI, "b", typedValue))

 def pSeq2FloatType: Pickler[String ~ Float] = 
    elem(TURI, "pair", 
        elem(TURI, "a", text) ~ elem(TURI, "b", typedValue))

def pSeq2DoubleType: Pickler[String ~ Double] = 
    elem(TURI, "pair", 
        elem(TURI, "a", text) ~ elem(TURI, "b", typedValue))

def pSeq2BooleanType: Pickler[String ~ Boolean] = 
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
}

import _root_.org.specs2.mutable._


/** 
 * This class tests simple XML pickler combinators.
 *
 * @author Iulian Dragos (iuliandragos@google.com) 
 * @author Richard Searle
 */

object PicklerTest extends  PicklerAsserts{
  
import definitions._
import Picklers._


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

val inputNoNS =
    """<pair>
<a>alfa</a>
<b>omega</b>
</pair>
"""


val mixed =
    """<pair xmlns="testing-uri">Mixed</pair>
"""

val optMixed =
    """<pair xmlns="testing-uri">Mixed<a>alfa</a></pair>
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

val inputBoolean =
    """<pair xmlns="testing-uri">
<a>alfa</a>
<b>true</b>
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
  val pairDouble = new ~("alfa", 12.34)
  val pairBoolean = new ~("alfa",true)
  val pairOptNone = new ~("alfa",None)
  val pairOptSome = new ~("alfa",Some("omega"))
  val pairNestedSeq = (new ~("alfa", "omega")) ~ (new ~("beta", "phi"))
  val pairListSingle = List(new ~("alfa", "omega"))
  val pairSetSingle = Set(new ~("alfa", "omega"))
  val pairMapSingle = Map("alfa"->new ~("alfa", "omega"))
  val pairListTwo = List(new ~("alfa", "omega"), new ~("delta","gamma"))
  val pairMapTwo = Map("alfa"->new ~("alfa", "omega"),"delta"->new ~("delta", "gamma"))
  val pairSetTwo = Set(new ~("alfa", "omega"), new ~("delta","gamma"))
  

"Picklers " should {
 
"testSequenceNoNSUnpickle" in  {
    assertSucceedsWith("Sequence unpickling failed", pair, inputNoNS, pSeqNoNS2)
  }

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

"testSequenceMarkerPickle" in  {
 val pickled = pSeqMarker2.pickle(new ~("a",true))
 val expected = """<pair xmlns="testing-uri">
<a>a</a>
<b/>
</pair>
"""
       normalize(expected) must beEqualTo(normalize(pickled))
 }

"testSequenceLoggedPickle" in  {
 val pickled = pSeq2Logged.pickle(pair)
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

"testMapJoinPickle" in  {
 val pickled = pMapJoin2.pickle(pairMapSingle)
       normalize(input) must beEqualTo(normalize(pickled))
 }

"testMapTwoSequencePickle" in  {
 val pickled = pMapSeq2.pickle(pairMapTwo)
       normalize(inputMultiple) must beEqualTo(normalize(pickled))
 }

"testMapTwoJoinPickle" in  {
 val pickled = pMapJoin2.pickle(pairMapTwo)
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

"testMixedUnpickleText" in  {
    assertSucceedsWith("testMixedUnpickleText", "Mixed", mixed, pMixed)
  }

"testSeqMarker2" in  {
    assertSucceedsWith("testMixedUnpickleText", new ~("alfa",true), input, pSeqMarker2)
  }

"testMixedUnpickleElem" in  {
    assertSucceedsWith("testMixedUnpickleText", "alfa", input, pMixed)
  }

 "testOptMixedUnpickleElem" in  {
    assertSucceedsWith("testMixedUnpickleText", new ~(Some("Mixed"), "alfa"), optMixed, pOptMixed)
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
      case f: NoSuccess  =>  failure(f.toString)
    }
     val pickled = pXml.pickle(result.get)
       normalize(input) must beEqualTo(normalize(pickled))
  }

"testPath" in  {
   val in="""<Profile  xmlns="testing-uri"> 
<entry>
<value>v</value>
</entry>
</Profile>
"""
    assertSucceedsWith("Path unpickling failed", "v", in, pPath)
  }

"testPathWhen" in  {
   val in="""<Profile  xmlns="testing-uri"> 
<entry>
<kind>a</kind>
<value>v</value>

</entry>
<entry>
<kind>b</kind>
<value>w</value>

</entry>
</Profile>
"""
    assertSucceedsWith("Path unpickling failed", "w", in, pPathWhen)
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

"testMapJoinUnpickle" in  {
    assertSucceedsWith("Sequence unpickling failed", pairMapSingle, input, pMapJoin2)
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

"testMapTwoJoinUnpickle" in  {
    assertSucceedsWith("Sequence unpickling failed", pairMapTwo, inputMultiple, pMapJoin2)
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

"testSequenceDoubleTypeUnpickle" in  {
    assertSucceedsWith("Sequence unpickling failed", pairDouble, inputFloat, pSeq2DoubleType)
  }

"testSequenceBooleanTypeUnpickle" in  {
    assertSucceedsWith("Sequence unpickling failed", pairBoolean, inputBoolean, pSeq2BooleanType)
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


   "testSwitchABUnpickle" in {
    val in =
      """<p:strings xmlns:p="testing-uri">
<p:str kind="ab">
<p:a>a</p:a>
<p:b>b</p:b>
</p:str>
</p:strings>
"""    
    
    val expected = new ~("a", "b") 
    assertSucceedsWith("Unpickling switch AB", expected, in, pSwitch)
  }

"testSwitchCDUnpickle" in {
    val in =
      """<p:strings xmlns:p="testing-uri">
<p:str kind="cd">
<p:c>12</p:c>
<p:d>d</p:d>
</p:str>
</p:strings>
"""    
    
    val expected = new ~(12, "d") 
    assertSucceedsWith("Unpickling switch CD", expected, in, pSwitch)
  }


  "testSwitchCDPickle" in {
    // TODO no kind
    val in =
      """<strings xmlns="testing-uri">
<str>
<c>12</c>
<d>d</d>
</str>
</strings>
"""    
    
    val expected = new ~(12, "d") 
    val pickled = pSwitch.pickle(expected)
    normalize(in) must beEqualTo( normalize(pickled))
  }

   "testWhen" in {
    val expected = new ~("this is special", "one") ~ "a" ~ "b"
    assertSucceedsWith("Unpickling when", expected, inputSpecial, pWhen)
  }


"testWhenNoSpecial" in {
  
    val input =
      """<p:strings xmlns:p="testing-uri">
<p:st>one</p:st>
<p:str kind="other">not special</p:str>
<p:a>a</p:a>
<p:b>b</p:b>
<p:str kind="xxx">this is special</p:str>

</p:strings>
"""    
    val expected = new ~("not special", "one") ~ "a" ~ "b"
    assertSucceedsWith("Unpickling when", expected, input, pWhen)
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
  

}

