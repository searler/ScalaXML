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

import scala.xml.factory.XMLLoader
import scala.xml._
import org.xml.sax._
import org.xml.sax.helpers.DefaultHandler

class Loader extends DefaultHandler  with   XMLLoader[Elem]{

   val newAdapter = adapter
  
   def value = newAdapter.rootElem.asInstanceOf[Elem]

   override def characters( ch:Array[Char],start:Int,length:Int) {
     newAdapter.characters(ch,start,length)
   }
   
   override def endDocument() {
     newAdapter.scopeStack.pop
   }

   override def endElement(uri:String,localName:String, qName:String){
     newAdapter.endElement(uri,localName,qName)
   }
         
  
         
   override def processingInstruction(target:String,  data:String){
     newAdapter.processingInstruction(target,data)
   }
         
 
   override def startDocument(){
     newAdapter.scopeStack push TopScope
   }
         
   override def startElement(uri:String,localName:String, qName:String,atts:Attributes){
       newAdapter.startElement(uri,localName,qName,atts)
   }
         

         
}