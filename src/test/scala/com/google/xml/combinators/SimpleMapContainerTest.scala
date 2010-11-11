package com.google.xml.combinators

import org.specs._



import scala.xml.PrettyPrinter

import Picklers._



class MapContainedTest  extends PicklerAsserts{

   


 "parseContained" in {
    val in = """<container xmlns="testing-uri">
<name>name</name>
<key>tagged</key>
<value>123</value>
</container>"""
            
     val result = SimpleMapContainer.pickler.unpickle(LinearStore.fromString(in))
     
      result match {
      case Success(v, _) => SimpleMapContainer("name",Map("tagged"->MapContained("tagged",123))) must beEqualTo(v)
      case f: NoSuccess  => fail(f toString)
    }
}
 



 "unparseContained" in {
 val r=  SimpleMapContainer("name",Map("tagged"->MapContained("tagged",123),"next"->MapContained("next",1623)))
    
    val out = PlainOutputStore.empty
    val xml=   SimpleMapContainer.pickler.pickle(r,out)
    """<container xmlns="testing-uri">
<name>name</name>
<key>tagged</key>
<value>123</value>
<key>next</key>
<value>1623</value>
</container>
""" must beEqualTo(normalize(xml.document))

  }
      
  
  

}
