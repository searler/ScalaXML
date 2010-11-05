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

  def rawInternalPickler = elem("tag",text)(NURI)~elem("value",intVal)(NURI)
  def internalPickler: Pickler[Internal] = wrapCaseClass(rawInternalPickler) (Internal.apply) (Internal.unapply)

  def rawPickler  = 
    elem("rating", 
      elem("name",text)(TURI)  ~ elem("internal", internalPickler)(NURI) ~ rep(elem("max", intVal)(TURI))
	  )(URI("http://schemas.google.com/g/2005"))
  
   

  def pickler: Pickler[Nested] = wrapCaseClass(rawPickler) (Nested.apply) (Nested.unapply)
}