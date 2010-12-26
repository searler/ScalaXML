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
import javax.xml.namespace.QName


object NullList extends org.w3c.dom.NodeList{
   def getLength = 0
   def item(index:Int):org.w3c.dom.Node = null
}

class SingletonList(node:org.w3c.dom.Node) extends org.w3c.dom.NodeList{
   def getLength = 1
   def item(index:Int):org.w3c.dom.Node = node
}

class Wrapper[T](val value:T) extends org.w3c.dom.NodeList with org.w3c.dom.Node {
  import  org.w3c.dom._
   def getLength = 1
   def item(index:Int):org.w3c.dom.Node = this
   def appendChild(newChild:Node):Node = null
   def cloneNode(deep:Boolean):Node = this
   def getAttributes():NamedNodeMap = null
   def getChildNodes():NodeList = new SingletonList(this)
   def getFirstChild():Node = this
   def getLastChild():Node = this
   def getLocalName() = ""
   def getNamespaceURI() = ""
   def getNextSibling():Node = null
   def getNodeName():String = ""
   def getNodeType():Short  = Node.TEXT_NODE
   def getParentNode():Node = null
   def getPrefix():String = null
   def getPreviousSibling():Node = null
   def hasAttributes():Boolean = false
   def hasChildNodes():Boolean = false
   def insertBefore(newChild:Node,refChild:Node):Node = null
   def isSupported(feature:String,version:String):Boolean = false
   def getFeature(feature:String,version:String):Object = null
   def normalize() {}
   def removeChild(oldChild:Node):Node = oldChild
   def replaceChild(newChild:Node,oldChild:Node):Node = oldChild
   def setNodeValue(nodeValue:String) { }
   def getNodeValue():String = value toString
   def setPrefix(prefix:String) { }
   def getTextContent():String = value toString
   def setTextContent(textContent:String) { }
   def setUserData(key:String,data:Object,handler:UserDataHandler):Object = null
   def getUserData(key:String):Object  = null
   def getOwnerDocument():Document = null
   def getBaseURI():String = null
   def isEqualNode(other:Node):Boolean = false
   def isSameNode(other:Node):Boolean = other eq this
   def isDefaultNamespace(namespaceURI:String):Boolean = false
   def lookupNamespaceURI(prefix:String):String = null
   def lookupPrefix(namespaceURI:String):String = null
   def compareDocumentPosition(other:Node):Short = 0
   
}

trait Arity extends XPathFunction {
  
  def arity:Int
  def to[T:Convert](args:java.util.List[_],index:Int):T =  try { 
    implicitly[Convert[T]].convert(args.get(index))
  } catch {
     case t @ _ => { SimplerLogger("convert",t);  throw t}
  }  
  
}

trait Convert[T] {
   def primitiveType:javax.xml.namespace.QName 
   def convert(a:Any):T
   def viaString(a:Any):T = {
    a match {  
              case w:Wrapper[T] => w.value
              case  nl:org.w3c.dom.NodeList if nl.getLength == 0 => convert(null)
              case  nl:org.w3c.dom.NodeList if nl.getLength == 1 =>  convert(nl.item(0).getTextContent)
              case  nl:org.w3c.dom.NodeList => { val sb = new StringBuilder
	        for(i<-0 until nl.getLength) sb.append(nl.item(i).getTextContent)
	        convert(sb.toString) 
	      }
              case n:org.w3c.dom.Node => convert(n getTextContent)
              case null => convert(null)		
              case x @ _ => convert(x.toString)
       }
    }  
}

object Converters{

implicit object  IntConvert extends Convert[Int] { 
    val primitiveType = javax.xml.xpath.XPathConstants.NUMBER
    def convert(a:Any):Int = a match  {
	    case i:Int => i
	    case d:Double => d toInt
	    case s:String => s toInt 
	    case b:Boolean => if(b)1 else 0  
	    case null => 0  
	    case x @ _ => viaString(x)  
}

}
implicit object  LongConvert extends Convert[Long] { 
   val primitiveType = javax.xml.xpath.XPathConstants.NUMBER
   def convert(a:Any):Long = a match  {
	    case l:Long => l
	    case d:Double => d toLong
	    case s:String => s toLong
	    case b:Boolean => if(b)1 else 0
	    case x @ _ => viaString(x)  
}

}

implicit object  DoubleConvert extends Convert[Double] { 
   val primitiveType = javax.xml.xpath.XPathConstants.NUMBER
  def convert(a:Any):Double = {
  a match{ 
    case d:Double => d
    case s:String =>  s toDouble   
    case b:Boolean => if(b)1 else 0
    case x @ _ => viaString(x) 
}
}}
implicit object StringConvert extends Convert[String] { 
   val primitiveType = javax.xml.xpath.XPathConstants.STRING
   def convert(a:Any):String = a match {
    case s:String => s
    case d:Double => d toString
    case b:Boolean => b toString
    case null => null
    case x @ _ => viaString(x)  
}
}

implicit object  BooleanConvert extends Convert[Boolean] { 
  val primitiveType = javax.xml.xpath.XPathConstants.BOOLEAN
    def convert(a:Any):Boolean = a match  {
	    case d:Double => d > 0
	    case s:String => s.length > 0
	    case b:Boolean => b
	    case  nl:org.w3c.dom.NodeList => nl.getLength > 0
	    case x @ _ => viaString(x)  
}

}

implicit object  DateConvert extends Convert[java.util.Date] { 
  val primitiveType = javax.xml.xpath.XPathConstants.STRING
   def convert(a:Any):java.util.Date = a match  {
	    case d:java.util.Date => d
	    case d:Double => new java.util.Date(d toLong)
	    case s:String => { val df = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss"); df.parse(s)}
	    case Boolean => throw new IllegalArgumentException("Cannot convert Boolean to date")  
	    case x @ _ => viaString(x)  
}

}



implicit object  ListConvert extends Convert[List[org.w3c.dom.Node]] { 
   val primitiveType = javax.xml.xpath.XPathConstants.NODESET
   def convert(a:Any):List[org.w3c.dom.Node] = a match  {
            case  nl:org.w3c.dom.NodeList if nl.getLength == 0 => Nil
            case  nl:org.w3c.dom.NodeList if nl.getLength == 1 =>  List(nl.item(0))
	    case  nl:org.w3c.dom.NodeList =>  { val lb = new scala.collection.mutable.ListBuffer[org.w3c.dom.Node];  
	           for(i<-0 until nl.getLength) lb += nl.item(i)
	           lb toList
	      }
              case n:org.w3c.dom.Node => List(n)
              case null => Nil	
	      case x @ _ => throw new IllegalArgumentException("Cannot convert " + x) 
}
}

implicit object  NodeConvert extends Convert[org.w3c.dom.Node] {
  val primitiveType = javax.xml.xpath.XPathConstants.NODE
  def convert(a:Any):org.w3c.dom.Node = a match  {
            case  nl:org.w3c.dom.NodeList if nl.getLength == 0 => null
            case  nl:org.w3c.dom.NodeList if nl.getLength == 1 => nl.item(0)
              case n:org.w3c.dom.Node => n
              case null => null	
	      case x @ _ => throw new IllegalArgumentException("Cannot convert " + x) 
}

}

}



class One[T:Convert,R](val f:Function1[T,R])  extends Arity  { 
 def evaluate(args:java.util.List[_]) = {  new Wrapper(f(to[T](args,0))) }
 val arity = 1
}

class Two[T:Convert,U:Convert,R](val f:Function2[T,U,R])  extends Arity {  
 def evaluate(args:java.util.List[_]) = {new Wrapper(f(to[T](args,0),to[U](args,1))) }
 val arity = 2
}

class Reference(f: => Object){def apply():Object = f}

object XPathFunctionDef {
   implicit def f1ToOne[T:Convert,R](f:Function1[T,R]) = new One(f)
   implicit def f2TTwo[T:Convert,U:Convert,R](f:Function2[T,U,R]) = new Two(f)
   implicit def toRef(v: => Object) = new Reference({v})
}




class VariableResolver(args:(String,Reference)*)(implicit nsc:Context = new Context()) extends XPathVariableResolver{
     val map:Map[QName,Reference] = Map((args.map { tup => nsc.qname(tup._1)->tup._2 }):_*)
     def resolveVariable(variableName:QName):Object = map(variableName)()
}


class FunctionResolver(args:(String,Arity)*)(implicit nsc:Context = new Context()) extends XPathFunctionResolver{
     val map:Map[(QName,Int),XPathFunction] = Map((args.map { tup => (nsc.qname(tup._1),tup._2.arity)->tup._2 }):_*)
     def resolveFunction(functionName:QName,arity:Int):XPathFunction =    map((functionName,arity))
}
