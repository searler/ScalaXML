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

import javax.xml.XMLConstants

import org.specs._

object nameSpaceTest extends Specification {
  
  "empty" in {
    val m= new Context()
    XMLConstants.XML_NS_URI must beEqualTo (m.getNamespaceURI("xml"))
    XMLConstants.NULL_NS_URI must beEqualTo (m.getNamespaceURI("xxxx"))
    "http://www.w3.org/2005/xpath-functions" must beEqualTo (m.getNamespaceURI("fn"))
    "{http://www.w3.org/XML/1998/namespace}local" must beEqualTo( m.qname("xml","local").toString())
    "{http://www.w3.org/XML/1998/namespace}local" must beEqualTo( m.qname("xml:local").toString())
  }
  
  "ex->http://example.com,goog->http://google.com" in {
    val m= new Context("ex"->"http://example.com","goog"->"http://google.com")
    "http://example.com" must beEqualTo (m.getNamespaceURI("ex"))
    "{http://example.com}local" must beEqualTo( m.qname("ex","local").toString())
    "{http://example.com}local" must beEqualTo( m.qname("ex:local").toString())
    "local" must beEqualTo( m.qname("local").toString())
    "http://google.com" must beEqualTo (m.getNamespaceURI("goog"))
    XMLConstants.XML_NS_URI must beEqualTo (m.getNamespaceURI("xml"))
    XMLConstants.NULL_NS_URI must beEqualTo (m.getNamespaceURI("xxxx"))
  }
  
}