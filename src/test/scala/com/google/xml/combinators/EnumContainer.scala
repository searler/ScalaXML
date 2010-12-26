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



case class  EnumContainer(s:String,e:TestEnum)

object EnumContainer{
 import Picklers._

 implicit object EnumConverter extends EnumConvert(classOf[TestEnum])

  final val TURI = URI("testing-uri")
  def rawPickler  = 
    elem("container", 
      elem("name",text)(TURI)  ~ elem("enum", typedValue)(TURI)
        )(TURI)
  
   

def pickler: Pickler[EnumContainer] = wrapCaseClass(rawPickler) (EnumContainer.apply) (EnumContainer.unapply)

}