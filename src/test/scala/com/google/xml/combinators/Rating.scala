package com.google.xml.combinators


case class Rating(
    name:String,
    count:Int,
    min: Int, 
    cost:Int,
    max: List[Int]
    ) {
}

object Rating {

  import Picklers._

  final val TURI = URI("testing-uri")
  def rawPickler  = 
    elem("rating", 
      elem("name",text)(TURI)  ~ attr("count", intVal) ~  attr("min", intVal) ~  default(attr("cost", intVal),666) ~ list(elem("max", intVal)(TURI))
        )(URI("http://schemas.google.com/g/2005"))
  
   

def pickler: Pickler[Rating] = wrapCaseClass(rawPickler) (Rating.apply) (Rating.unapply)

   
}
