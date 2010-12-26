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
package cognitiveentity.xml.combinators

trait Common

case class Internal(tag:String,value:Int) extends Common
case class Contained(tag:String,value:Int) extends Common
case class Single(tag:String) extends Common

object Internal{
import Picklers._
  def rawInternalPickler(implicit u:URI) = elem("internal", elem("tag",text)~elem("value",intVal))
  def internalPickler: Pickler[Internal] = wrapCaseClass(rawInternalPickler(URI("nested-uri"))) (Internal.apply) (Internal.unapply)
}

object Contained{
import Picklers._
  def rawPickler(implicit u:URI) =  elem("tag",text)~elem("value",intVal)
  def pickler = wrapCaseClass(rawPickler(URI("contained-uri"))) (Contained.apply) (Contained.unapply)
}

object Single{
import Picklers._
  def rawPickler(implicit u:URI) =  elem("tag",text)
  def pickler = wrapCaseClass(rawPickler(URI("contained-uri"))) (Single.apply) (Single.unapply)
}

case class Nested[T](
    name:String,
    internal:T,
    list: List[T]
    ) {}


object Variant{
  import Picklers._

 final val TURI = URI("testing-uri")

def discrim = elem(TURI,"value", attr("kind",text))

 def unpickle:PartialFunction[String, St =>PicklerResult[Common]] = {  
                  case "internal" => (discrim ~> Internal.internalPickler).unpickle
                  case "contained" =>  (discrim ~> Contained.pickler).unpickle
               }

  def pickle:PartialFunction[Common, XmlOutputStore => XmlOutputStore] = {
                          case i:Internal => {o => discrim.pickle("internal",o); Internal.internalPickler.pickle(i,o)}
                          case c:Contained => {o => discrim.pickle("contained",o);Contained.pickler.pickle(c,o)}
                       }

 def pickler:Pickler[Common] = elem(TURI, "variant",
                       switch(discrim, unpickle, pickle)
                     )

}


object Nested {
 
  import Picklers._
  
  final val TURI = URI("testing-uri")
   

  def rawPickler[T](ip:Pickler[T])  = 
    elem("rating", 
      elem("name",text)(TURI)  ~ ip ~ list(ip)
	  )(URI("http://schemas.google.com/g/2005"))
  
  private def fromNested[T](n:Nested[T]) = tuple3Unapply(Nested.unapply[T](n))

  def pickler[T](ip:Pickler[T]): Pickler[Nested[T]] = wrapCaseClass(rawPickler[T](ip)) (Nested.apply[T]) (fromNested[T])
}