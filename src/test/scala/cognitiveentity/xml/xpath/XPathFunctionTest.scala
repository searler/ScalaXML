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

import org.specs._

import javax.xml.xpath.XPathFunction
import scala.collection.JavaConversions._
import javax.xml.namespace.QName

object xpathFunction extends Specification {

import cognitiveentity.xml.xpath.XPathFunctionDef._
import cognitiveentity.xml.xpath.Converters._
   
//   implicit object  IntConvert extends Convert[Int] { def convert(a:Any):Int =java.lang.Integer.parseInt(a toString)}
//implicit object StringConvert extends Convert[String] { def convert(a:Any):String =a toString}

 "single int" in {
     val m= Map[String,XPathFunction]("a1"->{i:Int=>i*2})
     24 must beEqualTo (m("a1").evaluate(List(12)).asInstanceOf[Wrapper[Int]].value) 
  }

 "two string int" in {
     val m= Map[String,XPathFunction]("a2"->{(s:String,i:Int)=>s*i})
     "xyxy" must beEqualTo (m("a2").evaluate(List("xy",2)).asInstanceOf[Wrapper[String]].value) 
  }

  "via resolver" in {
     val r= new FunctionResolver("a1"->{i:Int=>i*2}, "a1"->{(s:String,i:Int)=>s*i})
     24 must beEqualTo (r.resolveFunction(new QName("a1"),1).evaluate(List(12)).asInstanceOf[Wrapper[Int]].value) 
     "xyxy" must beEqualTo (r.resolveFunction(new QName("a1"),2).evaluate(List("xy",2)).asInstanceOf[Wrapper[String]].value)  
     
  }

"via resolver with nsc" in {
     implicit val nsc = new Context("ex"->"http://example.com")
     val r= new FunctionResolver("ex:a1"->{i:Int=>i*2}, "a1"->{(s:String,i:Int)=>s*i})
     24 must beEqualTo (r.resolveFunction(new QName("http://example.com","a1"),1).evaluate(List(12)).asInstanceOf[Wrapper[Int]].value)
     "xyxy" must beEqualTo (r.resolveFunction(new QName("a1"),2).evaluate(List("xy",2)).asInstanceOf[Wrapper[String]].value)  
     
  }

}