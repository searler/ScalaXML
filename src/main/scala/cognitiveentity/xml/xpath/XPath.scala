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

import javax.xml.xpath._
import org.xml.sax._
import java.io._



class XPath[T](s:String)(implicit tc:Convert[T],implicit val nsc:Context = new Context(),implicit val fun:FunctionResolver = new FunctionResolver(),implicit val variables:VariableResolver = new VariableResolver() ){
  val expr = XPath.compile(s,nsc,fun,variables)
  def apply(s:String):T = get(s).get
  def get(s:String):Option[T] = unapply(s)
  def unapply(s:String):Option[T] =  {
    try{
      val ns= (expr.evaluate(new InputSource(new StringReader(s)),XPathConstants.NODESET)).asInstanceOf[org.w3c.dom.NodeList]
      if(ns.getLength == 0)
	None
	else  
	  Some(tc.convert(ns))
    }catch{
      case t @ _ => SimplerLogger("unapply",t)
      None
    }
  }
  
  
}

class XPathPrimitive[T](s:String)(implicit tc:Convert[T],implicit val nsc:Context = new Context(),implicit val fun:FunctionResolver = new FunctionResolver(),implicit val variables:VariableResolver = new VariableResolver() ){
 
  val expr = XPath.compile(s,nsc,fun,variables)
   def apply(s:String):T = get(s).get
   def get(s:String):Option[T] = unapply(s)
   def unapply(s:String):Option[T] =  {
    try{
      val vs= (expr.evaluate(new InputSource(new StringReader(s)),tc.primitiveType))
      Some(tc.convert(vs))
    }catch{
      case t @ _ => SimplerLogger("primitive unapply",t)
      None
    }
  }
  
  
}


object XPath {
   
  val xpath =  XPathFactory.newInstance().newXPath() 
  def compile(s:String,nsc:Context,fun:FunctionResolver,variables:VariableResolver) = { 
    try{
    xpath.setNamespaceContext(nsc)
    xpath.setXPathVariableResolver(variables)
    xpath.setXPathFunctionResolver(fun) 
    xpath.compile(s)
    }
    catch {
     case t @ _ => { SimplerLogger("compile",t);  throw t}
  }  
  }
}
