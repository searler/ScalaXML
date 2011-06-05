/* Copyright (c) 2010 Richard Searle
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/**
 * @author Richard Searle
 */
package cognitiveentity.xml.sax.interop

import _root_.org.specs2.mutable._

import java.io._
import javax.xml.parsers._
import javax.xml.transform._
import javax.xml.transform.sax._
import javax.xml.transform.stream._
import scala.xml._

object dumper extends Specification {


  "complex" in {    
    val xml = <Outside> <Y xmlns="http://y"/> <X a="12" c="21" xmlns:h="http://com.com" xmlns:q="http://q"  >xxx &lt; <y xmlns:g="http://example.com"  h:b="13"/> </X> </Outside>
    """<Outside> <Y xmlns="http://y"></Y> <X c="21" a="12" xmlns:q="http://q" xmlns:h="http://com.com">xxx &lt; <y h:b="13" xmlns:g="http://example.com"></y> </X> </Outside>""" must beEqualTo (xml.toString)
    """<?xml version="1.0" encoding="UTF-8"?><Outside> <Y xmlns="http://y"/> <X xmlns:q="http://q" xmlns:h="http://com.com" c="21" a="12">xxx &lt; <y xmlns:g="http://example.com" h:b="13"/> </X> </Outside>""" must beEqualTo (gen(xml))

  }
 
  "simple" in {
    val xml = <Outside></Outside>
    """<?xml version="1.0" encoding="UTF-8"?><Outside/>""" must beEqualTo(gen(xml) )
  }
  
  "comment" in {
    val xml = <Outside><!--comment--></Outside>
    """<?xml version="1.0" encoding="UTF-8"?><Outside><!--comment--></Outside>""" must beEqualTo(gen(xml) )
  }
  
  "simple with blank PCDATA" in {
    val xml = <Outside> </Outside>
    """<?xml version="1.0" encoding="UTF-8"?><Outside> </Outside>""" must beEqualTo(gen(xml) )
  }
  
  "simple with PI" in {
    val xml = <Outside><?name pidata?></Outside>
    """<Outside><?name pidata?></Outside>""" must beEqualTo(xml toString)
    """<?xml version="1.0" encoding="UTF-8"?><Outside><?name pidata?></Outside>""" must beEqualTo(gen(xml) )
  }
  
  "simple with CDATA" in {
    val xml = <Outside>{PCData("xxxx")}</Outside>
    """<Outside><![CDATA[xxxx]]></Outside>""" must beEqualTo(xml toString)
    """<?xml version="1.0" encoding="UTF-8"?><Outside><![CDATA[xxxx]]></Outside>""" must beEqualTo(gen(xml) )
  }
  
   "simple with &lt;" in {
    val xml = <Outside>&lt;</Outside>
    """<Outside>&lt;</Outside>""" must beEqualTo(xml toString)
    """<?xml version="1.0" encoding="UTF-8"?><Outside>&lt;</Outside>""" must beEqualTo(gen(xml) )
  }
  
   "simple with EntityRef" in {
    val xml = <Outside>{EntityRef("lt")}</Outside>
    """<Outside>&lt;</Outside>""" must beEqualTo(xml toString)
    """<?xml version="1.0" encoding="UTF-8"?><Outside>&lt;</Outside>""" must beEqualTo(gen(xml) )
  }
  
  private def gen(xml:scala.xml.Elem) =  {
    val transformerFactory = (TransformerFactory.newInstance).asInstanceOf[SAXTransformerFactory]
    val xformer = transformerFactory.newTransformerHandler
    val sw = new StringWriter
    xformer.setResult(new StreamResult(sw))
    
    val dumper = new XMLDumper(xformer)
    
    dumper.toXML(xml)
    
    sw.toString 
  }
  
}