package com.google.xml.combinators

import org.specs._



import scala.xml.PrettyPrinter

import Picklers._

class RatingTest  extends PicklerAsserts{

     "parse" in {
    val in = """<rating xmlns="http://schemas.google.com/g/2005"
                  min="12" cost="14" count="9">
                    <name xmlns="testing-uri">name</name>
                    <max xmlns="testing-uri">12</max>
                    <max xmlns="testing-uri">23</max>
                 </rating>"""
            
     val result = Rating.pickler.unpickle(LinearStore.fromString(in))
     
      result match {
      case Success(v, _) => Rating("name",9,12,14,12::23::Nil) must beEqualTo(v)
      case f: NoSuccess  => fail(f toString)
    }
}

  "parse no cost" in {
    val in = """<rating xmlns="http://schemas.google.com/g/2005"
                  min="13" count="9">
                    <name xmlns="testing-uri">name</name>
                    <max xmlns="testing-uri">12</max>
                    <max xmlns="testing-uri">23</max>
                 </rating>"""
            
     val result = Rating.pickler.unpickle(LinearStore.fromString(in))
     
      result match {
      case Success(v, _) => Rating("name",9,13,666,12::23::Nil) must beEqualTo(v)
      case f: NoSuccess  => fail(f toString)
    }
}


   "unparse" in {
 val r=  Rating("name",9,12,11,12::Nil)
    
    val out = PlainOutputStore.empty
    val xml=   Rating.pickler.pickle(r,out)
    """<gd:rating cost="11" count="9" min="12" xmlns:gd="http://schemas.google.com/g/2005">
<x:name xmlns:x="testing-uri">name</x:name>
<x:max xmlns:x="testing-uri">12</x:max>
</gd:rating>
""" must beEqualTo(normalize(xml.document))

  }
      
  
  

}
