package com.google.xml.combinators

import org.specs._



import scala.xml.PrettyPrinter

import Picklers._



class NestedTest  extends PicklerAsserts{

   

     "parseInternal" in {
    val in = """<rating xmlns="http://schemas.google.com/g/2005">
                    <name xmlns="testing-uri">name</name>
                    <internal xmlns="nested-uri">
                       <tag>tagged</tag>
                       <value>123</value>
                    </internal>
                 </rating>"""
            
     val result = Nested.pickler(Internal.internalPickler).unpickle(LinearStore.fromString(in))
     
      result match {
      case Success(v, _) => Nested("name",Internal("tagged",123),Nil) must beEqualTo(v)
      case f: NoSuccess  => fail(f toString)
    }
}


 "parseContained" in {
    val in = """<rating xmlns="http://schemas.google.com/g/2005">
                    <name xmlns="testing-uri">name</name>
                   
                       <tag xmlns="contained-uri">tagged</tag>
                       <value xmlns="contained-uri">123</value>
                   
                 </rating>"""
            
     val result = Nested.pickler(Contained.pickler).unpickle(LinearStore.fromString(in))
     
      result match {
      case Success(v, _) => Nested("name",Contained("tagged",123),Nil) must beEqualTo(v)
      case f: NoSuccess  => fail(f toString)
    }
}
 


   "unparseInternal" in {
 val r=  Nested("name",Internal("tagged",123),Internal("l1",111)::Nil)
    
    val out = PlainOutputStore.empty
    val xml=   Nested.pickler(Internal.internalPickler).pickle(r,out)
    """<rating xmlns="http://schemas.google.com/g/2005">
<name xmlns="testing-uri">name</name>
<internal xmlns="nested-uri">
<tag>tagged</tag>
<value>123</value>
</internal>
<internal xmlns="nested-uri">
<tag>l1</tag>
<value>111</value>
</internal>
</rating>
""" must beEqualTo(normalize(xml.document))

  }

 "unparseContained" in {
 val r=  Nested("name",Contained("tagged",123),Contained("l1",111)::Nil)
    
    val out = PlainOutputStore.empty
    val xml=   Nested.pickler(Contained.pickler).pickle(r,out)
    """<rating xmlns="http://schemas.google.com/g/2005">
<name xmlns="testing-uri">name</name>
<tag xmlns="contained-uri">tagged</tag>
<value xmlns="contained-uri">123</value>
<tag xmlns="contained-uri">l1</tag>
<value xmlns="contained-uri">111</value>
</rating>
""" must beEqualTo(normalize(xml.document))

  }
      
  
  

}
