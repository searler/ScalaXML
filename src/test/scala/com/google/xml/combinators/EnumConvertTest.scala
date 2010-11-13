package com.google.xml.combinators

import org.specs._



import Picklers._

object EnumConvertTest extends PicklerAsserts{
  
  "unpickle" in {
    val r = new EnumContainer("name",TestEnum.BETA)
   val out = PlainOutputStore.empty
    val xml=   EnumContainer.pickler.pickle(r,out)
    """<container xmlns="testing-uri">
<name>name</name>
<enum>BETA</enum>
</container>
""" must beEqualTo(normalize(xml.document))

   }

"pickle" in {
    val in = """<container xmlns="testing-uri">
<name>name</name>
<enum>BETA</enum>
</container>"""
            
     val result = EnumContainer.pickler.unpickle(LinearStore(in))
     
      result match {
      case Success(v, _) => EnumContainer("name",TestEnum.BETA) must beEqualTo(v)
      case f: NoSuccess  => fail(f toString)
    }
}

}