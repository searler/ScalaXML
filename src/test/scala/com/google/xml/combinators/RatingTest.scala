package com.google.xml.combinators

import org.specs._



import scala.xml.PrettyPrinter

import Picklers._
class RatingTest  extends Specification{

     "parse" in {
    val in = <rating xmlns="http://schemas.google.com/g/2005"
                  min="12" cost="14" count="9">
                    <name xmlns="testing-uri">name</name>
                    <max xmlns="testing-uri">12</max>
                 </rating>
            
     val result = Rating.pickler.unpickle(LinearStore.fromElem(in))
     
      result match {
      case Success(v, _) => Rating("name",9,12,14,12::Nil) must beEqualTo(v)
      case f: NoSuccess  =>
    }
}

   "unparse" in {
 val r=  Rating("name",9,12,11,12::Nil)
    
    val out = PlainOutputStore.empty
    val xml=   Rating.pickler.pickle(r,out)
    """<gd:rating cost="11" min="12" count="9" xmlns:gd="http://schemas.google.com/g/2005"><x:name xmlns:x="testing-uri">name</x:name><x:max xmlns:x="testing-uri">12</x:max></gd:rating>""" must beEqualTo(xml.rootNode.toString)

  }
      
  
  /*

	@Test def testParser(){
		val in = <rating xmlns="http://schemas.google.com/g/2005"
                  min="12" max="14" count="9">
                    <name xmlns="testing-uri">name</name>
                 </rating>
            
     val result = Rating.pickler.unpickle(LinearStore.fromElem(in))
     
      result match {
      case Success(v, _) =>  Assert.assertEquals(12,v.min)
      case f: NoSuccess  => Assert.fail(f.toString)
    }
    
	}
 
 @Test def testUnparser(){
     val r=  Rating("name",9,12,11,12::Nil)
    
    val out = PlainOutputStore.empty
    val xml=   Rating.pickler.pickle(r,out)
    Assert.assertEquals("""<gd:rating max="12" min="12" count="9" xmlns:gd="http://schemas.google.com/g/2005"><x:name xmlns:x="testing-uri">name</x:name></gd:rating>""",xml.rootNode.toString)
 }


*/

}
