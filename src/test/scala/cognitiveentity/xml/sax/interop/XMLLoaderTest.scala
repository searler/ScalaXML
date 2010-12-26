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

import org.specs._


import java.io._
import javax.xml.parsers._
import javax.xml.transform._
import javax.xml.transform.sax.SAXResult
import javax.xml.transform.stream.StreamSource


object loader extends Specification {
  
  "simple" in { 
    "<X><y></y>fdgfd</X>" must beEqualTo(gen("<X><y></y>fdgfd</X>") )
  }
  
  "&lt;" in { 
    "<X>&lt;</X>" must beEqualTo(gen("<X>&lt;</X>") )
  }
  
   "CDATA" in { 
    "<X>xxxx</X>" must beEqualTo(gen("<X><![CDATA[xxxx]]></X>") )
  }
  
  "CDATA required" in { 
    "<X>x&amp;y</X>" must beEqualTo(gen("<X><![CDATA[x&y]]></X>") )
  }
  
  "complex" in { 
    """<X xmlns="http://example.com"><y></y>fdgfd</X>""" must beEqualTo(gen("<X xmlns='http://example.com'><y></y>fdgfd</X>") )
  }
  
  
  def gen(xml:String) = {
    val transformerFactory = TransformerFactory.newInstance
    val xformer = transformerFactory.newTransformer
    val xl = new Loader
    xformer.transform(new StreamSource(new StringReader(xml)), new SAXResult(xl))
    //beEqualTo applied to Elem is always true, so have to use string representation
    xl.value.toString
  }
  
}