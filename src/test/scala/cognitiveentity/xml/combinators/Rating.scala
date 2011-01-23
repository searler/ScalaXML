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

/**
 * Derived from gdata-scala test case
 */
case class Rating(
    name:String,
    count:Int,
    min:Int, 
    cost:Int,
    max:List[Int]) 

object Rating {
   import Picklers._

   private final val TURI = URI("testing-uri")
   private def rawPickler  = 
    elem("rating", 
         elem("name",text)(TURI)  ~ attr("count", intVal) ~  attr("min", intVal) ~  default(attr("cost", intVal),666) ~ list(elem("max", intVal)(TURI))
        )(URI("http://schemas.google.com/g/2005"))
  
   def pickler: Pickler[Rating] = wrapCaseClass(rawPickler) (Rating.apply) (Rating.unapply) 
}
