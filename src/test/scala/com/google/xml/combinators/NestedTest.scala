package com.google.xml.combinators

import org.specs._



import scala.xml.PrettyPrinter

import Picklers._



class NestedTest  extends PicklerAsserts{

   

     "parse" in {
    val in = """<rating xmlns="http://schemas.google.com/g/2005">
                    <name xmlns="testing-uri">name</name>
                    <internal xmlns="nested-uri">
                       <tag>tagged</tag>
                       <value>123</value>
                    </internal>
                 </rating>"""
            
     val result = Nested.pickler.unpickle(LinearStore.fromString(in))
     
      result match {
      case Success(v, _) => Nested("name",Internal("tagged",123),Nil) must beEqualTo(v)
      case f: NoSuccess  => fail(f toString)
    }
}

 


   "unparse" in {
 val r=  Nested("name",Internal("tagged",123),12::Nil)
    
    val out = PlainOutputStore.empty
    val xml=   Nested.pickler.pickle(r,out)
    """<rating xmlns="http://schemas.google.com/g/2005">
<name xmlns="testing-uri">name</name>
<internal xmlns="nested-uri">
<tag>tagged</tag>
<value>123</value>
</internal>
<max xmlns="testing-uri">12</max>
</rating>
""" must beEqualTo(normalize(xml.document))

  }
      
  
  

}
