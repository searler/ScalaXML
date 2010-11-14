package com.google.xml.combinators

trait Common

case class Internal(tag:String,value:Int) extends Common
case class Contained(tag:String,value:Int) extends Common

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

case class Nested[T](
    name:String,
    internal:T,
    list: List[T]
    ) {}

case class Variant(value:Common)

object Variant{
  import Picklers._

 final val TURI = URI("testing-uri")

 def unpickle:PartialFunction[String, St =>PicklerResult[Common]] = {  
                  case "internal" => (ignore(TURI,"value") <~ Internal.internalPickler).unpickle
                  case "contained" =>  (ignore(TURI,"value") <~ Contained.pickler).unpickle
               }

  def pickle:PartialFunction[Common, XmlOutputStore => XmlOutputStore] = {
                          case i:Internal => Internal.internalPickler.pickle(i,_)
                          case c:Contained => Contained.pickler.pickle(c,_)
                       }

 def pickler = elem(TURI, "variant",
                       switch(elem(TURI,"value", attr("kind",text)), 
                         unpickle, pickle)
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