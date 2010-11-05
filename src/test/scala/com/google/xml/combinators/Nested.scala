package com.google.xml.combinators

case class Internal(tag:String,value:Int)

case class Nested(
    name:String,
    internal:Internal,
    max: List[Int]
    ) {}

object Nested {
 
  import Picklers._

  final val TURI = URI("testing-uri")
  final val NURI = URI("nested-uri")

  def rawInternalPickler(implicit u:URI) = elem("internal", elem("tag",text)~elem("value",intVal))
  def internalPickler(u:URI): Pickler[Internal] = wrapCaseClass(rawInternalPickler(u)) (Internal.apply) (Internal.unapply)

  def rawPickler  = 
    elem("rating", 
      elem("name",text)(TURI)  ~ internalPickler(NURI) ~ rep(elem("max", intVal)(TURI))
	  )(URI("http://schemas.google.com/g/2005"))
  
   

  def pickler: Pickler[Nested] = wrapCaseClass(rawPickler) (Nested.apply) (Nested.unapply)
}