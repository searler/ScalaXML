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

import scala.xml._
import org.xml.sax.ext.LexicalHandler
import org.xml.sax._
import org.xml.sax.helpers._

import scala.xml.Utility._

class XMLDumper(target:ContentHandler){
    val lexical:Option[LexicalHandler] = if(target.isInstanceOf[LexicalHandler]) Some(target.asInstanceOf[LexicalHandler]) else None
    
     def toXML(x: Node){
       target.startDocument
       toXML(x,TopScope)
       target.endDocument
     } 
	  
  private def toXML(x: Node,
	    pscope: NamespaceBinding)
	    {  
	      x match {
		case c: Comment => lexical.map(_.comment(c.commentText.toCharArray,0,c.commentText.length))
		case a: PCData => lexical.map { lex=> lex.startCDATA; characters(a); lex.endCDATA}
		case a: Atom[_] =>  characters(a)
		case e: EntityRef =>  characters(e)
		case p: ProcInstr => target.processingInstruction(p.target,p.proctext)
		case g: Group =>  g.nodes foreach {toXML(_,  x.scope)}
		case _  =>  element(x,x.scope,pscope )
	      }
	    }
	    
	  private def characters(n:Node){
	     val s = n text;
	     target.characters(s.toCharArray,0,s.length)
	  }
	    
	  private  def element(x:Node,ns:NamespaceBinding,stop:NamespaceBinding){
	      if(ns eq stop) {
		val uri = x.namespace
		val localName = x.label
		val qName = if(x.prefix == null) localName else x.prefix + ":" + localName
		target.startElement(uri,localName,qName,toAttributes(x) ) 
		x.child foreach {toXML(_,  x.scope)}
		target.endElement(uri,localName,qName)
	      }
	      else  {
		val p = if(ns.prefix==null) "" else ns.prefix
		target.startPrefixMapping(p,ns.uri)
		element(x,ns.parent, stop)
		target.endPrefixMapping(p)
	      }
	    }
	    
	  private  def toAttributes(node:Node) = {
	      val atts = new AttributesImpl() 
	      var natt = node.attributes
	      while(natt != null){
		natt match  {
		  case pa:PrefixedAttribute =>  atts.addAttribute(null,pa.key,pa.pre+":"+pa.key,"CDATA",toString(pa.value))
		  case ua:UnprefixedAttribute =>  atts.addAttribute(null,ua.key,ua.key,"CDATA",toString(ua.value))
		  case Null =>  //ignored
		}
		
		natt = natt.next
	      }
	      atts 
	    }
	    
	  private  def toString(ns:Seq[Node]) = {
	      val sb = new StringBuilder
	      ns foreach {_ match { 
		   case a: Atom[_] => sb.append(a.text)
		   case e:EntityRef => sb.append(e.text)
	         }
	      }
	      sb.toString
	    }
	    
} 