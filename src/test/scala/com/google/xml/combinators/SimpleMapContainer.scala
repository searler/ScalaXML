package com.google.xml.combinators


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
