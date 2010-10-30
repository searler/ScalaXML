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
  import com.google.gdata.data.Uris.gdNs
  import Picklers._

  final val URI = "testing-uri"
  def rawPickler  = 
    elem("rating", 
      elem("name",text)("x",URI)  ~ attr("count", intVal) ~  attr("min", intVal) ~  default(attr("cost", intVal),666) ~ rep(elem("max", intVal)("x",URI))
        )(gdNs)
  
   

def pickler: Pickler[Rating] = wrapCaseClass(rawPickler) (Rating.apply) (Rating.unapply)

   
}
