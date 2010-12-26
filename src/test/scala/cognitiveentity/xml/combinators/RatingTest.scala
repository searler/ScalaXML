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

import org.specs._



import Picklers._

class RatingTest  extends PicklerAsserts{

     "parse" in {
    val in = """<rating xmlns="http://schemas.google.com/g/2005"
                  min="12" cost="14" count="9">
                    <name xmlns="testing-uri">name</name>
                    <max xmlns="testing-uri">12</max>
                    <max xmlns="testing-uri">23</max>
                 </rating>"""
            
     val result = Rating.pickler.unpickle(LinearStore(in))
     
      result match {
      case Success(v, _) => Rating("name",9,12,14,12::23::Nil) must beEqualTo(v)
      case f: NoSuccess  => fail(f toString)
    }
}

  "parse no cost" in {
    val in = """<rating xmlns="http://schemas.google.com/g/2005"
                  min="13" count="9">
                    <name xmlns="testing-uri">name</name>
                    <max xmlns="testing-uri">12</max>
                    <max xmlns="testing-uri">23</max>
                 </rating>"""
            
     val result = Rating.pickler.unpickle(LinearStore(in))
     
      result match {
      case Success(v, _) => Rating("name",9,13,666,12::23::Nil) must beEqualTo(v)
      case f: NoSuccess  => fail(f toString)
    }
}


   "unparse" in {
 val r=  Rating("name",9,12,11,12::Nil)
    
    val out = PlainOutputStore.empty
    val xml=   Rating.pickler.pickle(r,out)
    """<rating cost="11" count="9" min="12" xmlns="http://schemas.google.com/g/2005">
<name xmlns="testing-uri">name</name>
<max xmlns="testing-uri">12</max>
</rating>
""" must beEqualTo(normalize(xml.document))

  }
      
  
  

}
