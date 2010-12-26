
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

import javax.xml.namespace._
import javax.xml.XMLConstants

class Context(args:(String,String)*) extends NamespaceContext{
   val map = Map[String,String]((args :+ ("xml",XMLConstants.XML_NS_URI) :+ ("fn","http://www.w3.org/2005/xpath-functions")):_*)
   def getNamespaceURI(prefix:String):String = map.get(prefix).getOrElse(XMLConstants.NULL_NS_URI)
   def getPrefix(namespaceURI:String):String = {throw new UnsupportedOperationException()}
   def getPrefixes(namespaceURI:String):java.util.Iterator[_] = {throw new UnsupportedOperationException()}
   def qname(prefix:String,localPart:String):QName = new QName(getNamespaceURI(prefix),localPart,prefix)
   def qname(prefixedLocal:String):QName =  {val pair = prefixedLocal.split(":"); if(pair.size==1)new QName(prefixedLocal) else qname(pair(0),pair(1))}

}