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
 * Has no runtime tests, since testing that the compiler 
 * makes the appropriate inferences.
 *
 * Note that type must be specified on all but last entry. (reason unknown at this time)
 *
 * @author Richard Searle
 */

object ConvertUsageTest extends Specification{
  import Picklers._
  import Converters._
  
  implicit final val TURI = URI("testing-uri")

 def pSeq3: Pickler[String ~ String ~ String] =
    elem(TURI, "triple",
        elem[String](TURI, "a")
      ~ elem[String](TURI, "b")
      ~ elem(TURI, "c"))

 def pIntConvert:Pickler[Int ~ Int] =  elem(TURI,"n",
                elem(TURI, "a")(IntConvert) ~ elem(TURI, "b")) 

 def pIntVal:Pickler[Int ~ Int] =  elem(TURI,"n",
                elem(TURI, "a",intVal) ~ elem(TURI, "b")) 

 def pInt:Pickler[Int ~ Int] =  elem(TURI,"n",
                elem[Int](TURI, "a") ~ elem(TURI, "b")) 

 def pIntImplicit:Pickler[Int ~ Int] =  elem("n",
                elem[Int]( "a") ~ elem[Int]( "b")) 

       
  def pNested2: Pickler[Int ~ Int] = 
    elem(TURI, "pair", 
            elem(TURI,"n",
                elem[Int](TURI, "a") ~ elem(TURI, "b")))   

 def pNestedSeq2: Pickler[(String ~ String) ~ (String ~ String)] = 
    elem(TURI, "pair", 
            elem(TURI,"n",
                elem[String](TURI, "a") ~ elem[String](TURI, "b")) ~ 
            elem(TURI,"o",
                elem[String](TURI, "c") ~ elem(TURI, "d"))
    )    


 def pSeq2Start : Pickler[String ~ String] = 
    elem(TURI, "pair", 
        ignore(TURI,"x") ~> elem[String](TURI, "a")   ~ elem(TURI, "b")) 

  def pSeq2Skip: Pickler[String ~ String] = 
    elem(TURI, "pair", 
        (elem[String](TURI, "a") <~ ignore(TURI,"x")) ~ elem(TURI, "b") )

  def pSeq2Int: Pickler[String ~ Int] = 
    elem(TURI, "pair", 
        elem[String](TURI, "a") ~ elem(TURI, "b"))

  def pSeqIntInt: Pickler[Int ~ Int] = 
    elem(TURI, "pair", 
        elem[Int](TURI, "a") ~ elem(TURI, "b"))

 
 def pSeq2IntType: Pickler[String ~ Int] = 
    elem(TURI, "pair", 
        elem[String](TURI, "a") ~ elem(TURI, "b"))

 def pSeq2FloatType: Pickler[String ~ Float] = 
    elem(TURI, "pair", 
        elem[String](TURI, "a") ~ elem(TURI, "b"))

 def pSeq2StringType: Pickler[String ~ String] = 
    elem(TURI, "pair", 
        elem[String](TURI, "a") ~ elem(TURI, "b"))

 def pSeq2Opt: Pickler[String ~Option[String]] = 
    elem(TURI, "pair", 
        elem[String](TURI, "a")  ~ opt(elem(TURI,"b")))

 def pSeq2Default: Pickler[String ~ String] = 
    elem(TURI, "pair", 
        elem[String](TURI, "a")  ~ default(elem(TURI,"b"),"omega"))

  def pSeq2 = 
    elem(TURI, "pair", 
        elem[String](TURI, "a") ~ elem[String](TURI, "b"))

}