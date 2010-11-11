package com.google.xml.combinators

import org.specs._



import scala.xml.PrettyPrinter

import Picklers._



class MapContainedTest  extends PicklerAsserts{

val in = """<container xmlns="testing-uri">
<name>name</name>
<key>tagged</key>
<value>123</value>
<key>next</key>
<value>1623</value>
</container>
"""

val value = SimpleMapContainer("name",Map("tagged"->MapContained("tagged",123),"next"->MapContained("next",1623)))
   


 "parseContained" in {
    
            
     val result = SimpleMapContainer.pickler.unpickle(LinearStore.fromString(in))
     
      result match {
      case Success(v, _) => value must beEqualTo(v)
      case f: NoSuccess  => fail(f toString)
    }
}
 



 "unparseContained" in {
    
    val out = PlainOutputStore.empty
    val xml=   SimpleMapContainer.pickler.pickle(value,out)
    in must beEqualTo(normalize(xml.document))

  }
      
  
  

}
