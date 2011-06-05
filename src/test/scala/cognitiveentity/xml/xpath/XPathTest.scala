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

import _root_.org.specs2.mutable._



object xpathTest extends Specification{

  import cognitiveentity.xml.xpath.XPathFunctionDef._
   
   import cognitiveentity.xml.xpath.Converters._

   implicit val nsc = new Context("pre"->"http://example.com")
   implicit val fun = new FunctionResolver("pre:node"->{n:org.w3c.dom.Node =>  n.getTextContent },
       "pre:list"->{l:List[org.w3c.dom.Node] => l.map(n => n.getTextContent).zipWithIndex.mkString },
       "pre:double"->{s:String =>s * 2 },
       "pre:mult"->{(s:String, i:Int) =>s * i },
       "pre:add"->{(s:String,f:Double) => s  + f},
       "pre:dt"->{d:java.util.Date => d })

"The xpath" should {
  
  "match" in {
    val xp = new XPath[String]("/x/y")
    "<x><y>text</y></x>" match {
      case xp(t) => "text" must beEqualTo(t) 
    }
  }
  
   "match nodes" in {
    val xp = new XPath[List[org.w3c.dom.Node]]("/x/y")
    "<x><y>text</y></x>" match {
      case xp(t) => "text" must beEqualTo(t(0).getTextContent) 
    }
  }
  
  "match()" in {
    val xp = new XPath[String]("/x/y")
    "text" must beEqualTo(xp( "<x><y>text</y></x>")) 
  }
  
  "match get" in {
    val xp = new XPath[String]("/x/y")
    Some("text") must beEqualTo(xp.get( "<x><y>text</y></x>")) 
  }
  
  "nomatch()" in {
    val xp = new XPath[String]("/x/z")
    xp( "<x><y>text</y></x>") must throwA[java.util.NoSuchElementException]
  }
  
  "nomatch get()" in {
    val xp = new XPath[String]("/x/z")
    None must beEqualTo( xp.get( "<x><y>text</y></x>") )
  }
  
   "multiple match" in {
    val xp = new XPath[String]("/x/y")
    "<x><y>a</y><y>b</y></x>" match {
      case xp(t) => "ab" must beEqualTo(t) 
    }
  }
  
   "node function " in {
     
   
    val xp = new XPath[String]("pre:node(/x/y)")
    "<x><y>a</y></x>" match {
      case xp(t) => "a" must beEqualTo(t) 
    }
  }
  
  
   "multiple match function " in {
     

    val xp = new XPath[String]("pre:list(/x/y)")
    "<x><y>a</y><y>b</y></x>" match {
      case xp(t) => "(a,0)(b,1)" must beEqualTo(t) 
    }
  }
  
  
  "variables constant" in {
     
     
      implicit val variables = new VariableResolver("pre:var"->"value")
      val xp = new XPathPrimitive[String]("$pre:var")
    "<x><y>text</y></x>" match {
      case xp(t) => "value" must beEqualTo(t) 
    }
  }
  
  "variables variable" in {
      var v ="xxx"
     
     
      implicit val variables = new VariableResolver("pre:var"->v)
      v = "value"
      val xp = new XPathPrimitive[String]("$pre:var")
      "<x><y>text</y></x>" match {
      case xp(t) => "value" must beEqualTo(t) 
      }
  }
  
  "variables explicit constant" in {
     
  
       val variables = new VariableResolver("pre:var"->"value")
      val xp = new XPathPrimitive[String]("$pre:var")(StringConvert,nsc,fun,variables)
    "<x><y>text</y></x>" match {
      case xp(t) => "value" must beEqualTo(t) 
    }
  }
  
  "std function" in {
    val xp = new XPathPrimitive[String]("concat(/x/y,'CAT')")
    "<x><y>text</y></x>" match {
      case xp(t) => "textCAT" must beEqualTo(t) 
    }
  }
  
  "ext function calls std function" in {
    
    val xp = new XPath[String]("pre:double(concat(/x/y,'CAT'))")
    "<x><y>text</y></x>" match {
      case xp(t) => "textCATtextCAT" must beEqualTo(t) 
    }
  }
  
   "std function calls ext" in {
    
  
    val xp = new XPathPrimitive[Int]("concat(/x/y,pre:double('12'))")
    "<x><y>23</y></x>" match {
      case xp(t) => 231212 must beEqualTo(t) 
    }
  }
  
  "std function with kind" in {
    val xp = new XPathPrimitive[Int]("concat(/x/y,'12')")
    "<x><y>23</y></x>" match {
      case xp(t) => 2312 must beEqualTo(t) 
    }
  }
  
  "std function with kind()" in {
    val xp = new XPathPrimitive[Int]("concat(/x/y,'12')")
    
     2312 must beEqualTo(xp("<x><y>23</y></x>")) 
    
  }
  
  "std function converting" in {
    val xp = new XPathPrimitive[Int]("concat(/x/y,'12')")
    "<x><y>23</y></x>" match {
      case xp(t) => 2312 must beEqualTo(t) 
    }
  }
  
  "nomatch" in {
    val xp = new XPath[String]("/x/z")
    "<x><y>text</y></x>" match {
      case xp(t) => failure("") 
      case i @ _ => "<x><y>text</y></x>" must beEqualTo(i)  
    }
  }
  
  
  
  
  "int match" in {
    val xp = new XPath[Int]("/x/y")
    "<x><y>123</y></x>" match {
      case xp(i:Int) => 123 must beEqualTo(i) 
    }
  }
  
  "int match()" in {
    val xp = new XPath[Int]("/x/y")
   
    123 must beEqualTo(xp("<x><y>123</y></x>")) 
   
  }
  
  "int nomatch" in {
    val xp = new XPath[Int]("/x/z")
    "<x><y>123</y></x>" match {
       case xp(t) => failure("")  
       case i @ _ => "<x><y>123</y></x>" must beEqualTo(i)   
    }
  }
  
   "namespace" in {
   
    val xp = new XPath[String]("/pre:x/pre:y")
    "<x xmlns='http://example.com'><y>text</y></x>" match {
      case xp(t) => "text" must beEqualTo(t) 
    }
  }
 
  "function" in {
   
   
   
    val xp = new XPath[String]("pre:double(/x/y)")
    "<x><y>text</y></x>" match {
      case xp(t) => "texttext" must beEqualTo(t) 
      case x @ _ => failure(x)
    }
  }
  
  "function(String,Double)" in {
   
   
    val xp = new XPath[String]("pre:add(/x/y,3)")
    "<x><y>text</y></x>" match {
      case xp(t) => "text3.0" must beEqualTo(t) 
    }
  }
  
  "function(String,Int)" in {
   
    val xp = new XPath[String]("pre:mult(/x/y,3)")
    "<x><y>text</y></x>" match {
      case xp(t) => "texttexttext" must beEqualTo(t) 
    }
  }
  
  "function(String,Int) from doc" in {
   
    val xp = new XPath[String]("pre:mult(/x/y,/x/i)")
    "<x><y>text</y><i>3</i></x>" match {
      case xp(t) => "texttexttext" must beEqualTo(t) 
    }
  }
  
  "function on attribute" in {
   
    val xp = new XPath[String]("pre:double(/x/y/@t)")
    "<x><y t='text' /></x>" match {
      case xp(t) => "texttext" must beEqualTo(t) 
    }
  }
  
   "xsd datetime function" in {
   
   
    val xp = new XPath[java.util.Date]("pre:dt(/x/y)")
    "<x><y>2006-08-09T13:26:50</y>></x>" match {
      case xp(t) => "Wed Aug 09 13:26:50 CDT 2006" must beEqualTo(t toString) 
    }
  }
  
   }
  
}