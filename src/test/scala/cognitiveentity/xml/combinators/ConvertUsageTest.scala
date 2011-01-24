/*
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
   
package cognitiveentity.xml.combinators

import org.specs._

/** 
 * This class tests convert definitions for combinators.
 *
 * Has no runtime tests, testing that the compiler 
 * makes the appropriate inferences.
 *
 * @author Richard Searle
 */

object ConvertUsageTest extends Specification{
  import Picklers._
  import Converters._
  
  final val TURI = URI("testing-uri")

       
  def pNested2: Pickler[String ~ String] = 
    elem(TURI, "pair", 
            elem(TURI,"n",
                elem(TURI, "a", text) ~ elem(TURI, "b", typedValue)))   

 def pNestedSeq2: Pickler[(String ~ String) ~ (String ~ String)] = 
    elem(TURI, "pair", 
            elem(TURI,"n",
                elem(TURI, "a", text) ~ elem(TURI, "b", text)) ~ 
            elem(TURI,"o",
                elem(TURI, "c", text) ~ elem(TURI, "d", text))
    )    


 def pSeq2Start : Pickler[String ~ String] = 
    elem(TURI, "pair", 
        ignore(TURI,"x") ~> elem(TURI, "a", text)   ~ elem(TURI, "b", typedValue)) 

  def pSeq2Skip: Pickler[String ~ String] = 
    elem(TURI, "pair", 
        (elem(TURI, "a", text) <~ ignore(TURI,"x")) ~ elem(TURI, "b", typedValue) )

  def pSeq2Int: Pickler[String ~ Int] = 
    elem(TURI, "pair", 
        elem(TURI, "a", text) ~ elem(TURI, "b", typedValue))

 
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
        elem(TURI, "a", text)  ~ opt(elem(TURI,"b",typedValue)))

 def pSeq2Default: Pickler[String ~ String] = 
    elem(TURI, "pair", 
        elem(TURI, "a", text)  ~ default(elem(TURI,"b",typedValue),"omega"))

}