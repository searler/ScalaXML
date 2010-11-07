package com.google.xml.combinators

case class Internal(tag:String,value:Int)
case class Contained(tag:String,value:Int)

object Internal{
import Picklers._
  def rawInternalPickler(implicit u:URI) = elem("internal", elem("tag",text)~elem("value",intVal))
  def internalPickler: Pickler[Internal] = wrapCaseClass(rawInternalPickler(URI("nested-uri"))) (Internal.apply) (Internal.unapply)
}

object Contained{
import Picklers._
  def rawPickler(implicit u:URI) = elem("contained", elem("tag",text)~elem("value",intVal))
  def pickler = wrapCaseClass(rawPickler(URI("contained-uri"))) (Contained.apply) (Contained.unapply)
}

case class Nested[T](
    name:String,
    internal:T,
    list: List[T]
    ) {}

object Nested {
 
  import Picklers._
  
  final val TURI = URI("testing-uri")
   

  def rawPickler[T](ip:Pickler[T])  = 
    elem("rating", 
      elem("name",text)(TURI)  ~ ip ~ rep(ip)
	  )(URI("http://schemas.google.com/g/2005"))
  
   def fromNested[T](n:Nested[T]) = Some((new ~(n.name, n.internal)) ~ n.list)

  def pickler[T](ip:Pickler[T]): Pickler[Nested[T]] = wrapCaseClass(rawPickler[T](ip)) (Nested.apply[T]) (fromNested[T])
}