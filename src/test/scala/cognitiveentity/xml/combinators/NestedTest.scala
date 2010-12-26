/* Copyright (c) 2010 Richard Searle
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

import org.specs._



import scala.xml.PrettyPrinter

import Picklers._



class NestedTest  extends PicklerAsserts{

  val pInternal =  Internal("tagged",123)
  val pContained = Contained("tagged",123)

 val inVariantInternal = """<variant xmlns="testing-uri">
<value kind="internal"/>
<internal xmlns="nested-uri">
<tag>tagged</tag>
<value>123</value>
</internal>
</variant>
"""

  val inVariantContained = """<variant xmlns="testing-uri">
<value kind="contained"/>
<tag xmlns="contained-uri">tagged</tag>
<value xmlns="contained-uri">123</value>
</variant>
"""


     "parseVariantInternal" in {
     val result:PicklerResult[Common] = Variant.pickler.unpickle(LinearStore(inVariantInternal))
     
      result match {
      case Success(v:Internal, _) =>pInternal must beEqualTo(v)
      case f: NoSuccess  => fail(f toString)
    }
}

  "unparseVariantInternal" in {
 
    
  
    val xml=   Variant.pickler.pickle(pInternal)
    inVariantInternal must beEqualTo(normalize(xml.document))

  }

"unparseVariantContaned" in {
    val xml=   Variant.pickler.pickle(pContained)
    inVariantContained must beEqualTo(normalize(xml.document))

  }


   

     "parseInternal" in {
    val in = """<rating xmlns="http://schemas.google.com/g/2005">
                    <name xmlns="testing-uri">name</name>
                    <internal xmlns="nested-uri">
                       <tag>tagged</tag>
                       <value>123</value>
                    </internal>
                 </rating>"""
            
     val result = Nested.pickler(Internal.internalPickler).unpickle(LinearStore(in))
     
      result match {
      case Success(v, _) => Nested("name",Internal("tagged",123),Nil) must beEqualTo(v)
      case f: NoSuccess  => fail(f toString)
    }
}

"parseVariantContained" in {
  
            
     val result = Variant.pickler.unpickle(LinearStore(inVariantContained))
     
      result match {
      case Success(v, _) => pContained must beEqualTo(v)
      case f: NoSuccess  => fail(f toString)
    }
}


 "parseContained" in {
    val in = """<rating xmlns="http://schemas.google.com/g/2005">
                    <name xmlns="testing-uri">name</name>
                   
                       <tag xmlns="contained-uri">tagged</tag>
                       <value xmlns="contained-uri">123</value>
                   
                 </rating>"""
            
     val result = Nested.pickler(Contained.pickler).unpickle(LinearStore(in))
     
      result match {
      case Success(v, _) => Nested("name",Contained("tagged",123),Nil) must beEqualTo(v)
      case f: NoSuccess  => fail(f toString)
    }
}
 


 "parseSingle" in {
    val in = """<rating xmlns="http://schemas.google.com/g/2005">
                    <name xmlns="testing-uri">name</name>
                       <tag xmlns="contained-uri">tagged</tag>
                 </rating>"""
            
     val result = Nested.pickler(Single.pickler).unpickle(LinearStore(in))
     
      result match {
      case Success(v, _) => Nested("name",Single("tagged"),Nil) must beEqualTo(v)
      case f: NoSuccess  => fail(f toString)
    }
}
 

   "unparseInternal" in {
 val r=  Nested("name",pInternal,Internal("l1",111)::Nil)
    
  
    val xml=   Nested.pickler(Internal.internalPickler).pickle(r)
    """<rating xmlns="http://schemas.google.com/g/2005">
<name xmlns="testing-uri">name</name>
<internal xmlns="nested-uri">
<tag>tagged</tag>
<value>123</value>
</internal>
<internal xmlns="nested-uri">
<tag>l1</tag>
<value>111</value>
</internal>
</rating>
""" must beEqualTo(normalize(xml.document))

  }

 "unparseContained" in {
 val r=  Nested("name",pContained,Contained("l1",111)::Nil)
    
  
    val xml=   Nested.pickler(Contained.pickler).pickle(r)
    """<rating xmlns="http://schemas.google.com/g/2005">
<name xmlns="testing-uri">name</name>
<tag xmlns="contained-uri">tagged</tag>
<value xmlns="contained-uri">123</value>
<tag xmlns="contained-uri">l1</tag>
<value xmlns="contained-uri">111</value>
</rating>
""" must beEqualTo(normalize(xml.document))

  }

"unparseSingle" in {
 val r=  Nested("name",Single("xx"),Single("l1")::Nil)
    
  
    val xml=   Nested.pickler(Single.pickler).pickle(r)
    """<rating xmlns="http://schemas.google.com/g/2005">
<name xmlns="testing-uri">name</name>
<tag xmlns="contained-uri">xx</tag>
<tag xmlns="contained-uri">l1</tag>
</rating>
""" must beEqualTo(normalize(xml.document))

  }


  

}
