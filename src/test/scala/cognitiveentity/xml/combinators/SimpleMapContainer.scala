/* Copyright (c) 2010 Richard Searle
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


case class MapContained(tag:String,value:Int)

object MapContained{
import Picklers._

 final val TURI = URI("testing-uri")
  def rawPickler =  elem("key",text)(TURI)~elem("value",intVal)(TURI)
  def pickler = wrapCaseClass(rawPickler) (MapContained.apply) (MapContained.unapply)
}


case class SimpleMapContainer(
    name:String,
    values: Map[String,MapContained]
    ) {
}


object SimpleMapContainer {

  import Picklers._

  final val TURI = URI("testing-uri")
  def rawPickler  = 
    elem("container", 
      elem("name",text)(TURI)  ~ map(keyOnly(elem(TURI, "key", text), MapContained.pickler))
        )(TURI)
  
   

def pickler: Pickler[SimpleMapContainer] = wrapCaseClass(rawPickler) (SimpleMapContainer.apply) (SimpleMapContainer.unapply)

   
}
