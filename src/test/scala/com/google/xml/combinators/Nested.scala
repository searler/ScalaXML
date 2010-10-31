package com.google.xml.combinators

case class Internal(tag:String,value:Int)

case class Nested(
    name:String,
    internal:Internal,
    max: List[Int]
    ) {}

object Nested {
  import com.google.gdata.data.Uris.gdNs
  import Picklers._

  final val URI = "testing-uri"

  def rawInternalPickler = elem("tag",text)("x",URI)~elem("value",intVal)("x",URI)
  def internalPickler: Pickler[Internal] = wrapCaseClass(rawInternalPickler) (Internal.apply) (Internal.unapply)

  def rawPickler  = 
    elem("rating", 
      elem("name",text)("x",URI)  ~ elem("internal", internalPickler)("x",URI) ~ rep(elem("max", intVal)("x",URI))
        )(gdNs)
  
   

  def pickler: Pickler[Nested] = wrapCaseClass(rawPickler) (Nested.apply) (Nested.unapply)
}