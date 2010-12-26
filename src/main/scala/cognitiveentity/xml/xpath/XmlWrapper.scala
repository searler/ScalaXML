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
package cognitiveentity.xml.xpath

import javax.xml.parsers._
import org.xml.sax ._
import java.io._

class XmlWrapper(val string:String) {
  
   var document:org.w3c.dom.Document = _
   var nodes:scala.xml.NodeSeq = _

   def doc:org.w3c.dom.Document = {
      if(document == null){
        if(nodes == null){
            val factory =  DocumentBuilderFactory.newInstance
            factory.setNamespaceAware(true)
            document =factory.newDocumentBuilder.parse(new InputSource(new StringReader(string)))
        }
      }
      document
   }
}

object XmlWrapper {
   implicit def stringToWrapper(s:String):XmlWrapper = new XmlWrapper(s)
}