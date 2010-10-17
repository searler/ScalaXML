package com.google.xml.combinators

object RatingDriver {
  def main(args : Array[String]) : Unit = {
    
    val in = <rating xmlns="http://schemas.google.com/g/2005"
                  min="12"  count="9">
                  <name xmlns="testing-uri">name</name>
                    <max xmlns="testing-uri">1</max>
                     <max xmlns="testing-uri">2</max>
             </rating>
                  
             
            
     val result = Rating.pickler.unpickle(LinearStore.fromElem(in))
    
    println(result)
    
    val r=  Rating("name",9,12,-1,12::13::Nil)
    
    val out = PlainOutputStore.empty
   val xml=   Rating.pickler.pickle(r,out)
    println(xml.rootNode)
    
   
     println(Rating.unapply(r))
  }
}
