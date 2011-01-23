
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
package cognitiveentity.xml.combinators

import org.specs._

import scala.xml.PrettyPrinter

import Picklers._

class SOAPTest  extends PicklerAsserts{

 /**
  * SOAP fault with a Detail entry, using an application specific namespace
  * 
  */  
   val inFault = """<env:Envelope xmlns:env="http://www.w3.org/2003/05/soap-envelope"
              xmlns:m="http://www.example.org/timeouts"
              xmlns:xml="http://www.w3.org/XML/1998/namespace">
 <env:Body>
  <env:Fault>
   <env:Code>
     <env:Value>env:Sender</env:Value>
     <env:Subcode>
      <env:Value>m:MessageTimeout</env:Value>
     </env:Subcode>
   </env:Code>
  
   <env:Reason>
     <env:Text xml:lang="en">Sender Timeout</env:Text>
      <env:Text xml:lang="af">Besender tuid</env:Text>
   </env:Reason>
   
    <env:Node>http://jenkov.com/theNodeThatFailed</env:Node>

  <env:Role>http://www.w3.org/2003/05/soap-envelope/role/ultimateReceiver</env:Role>
   
   <env:Detail m:issue="timing">
     <m:MaxTime>P5M</m:MaxTime>
   </env:Detail>    
  </env:Fault>
 </env:Body>
</env:Envelope>
"""		
 
/**
  * SOAP fault with a Detail entry that is a naked string
  * 
  */  
   val inFaultString = """<env:Envelope xmlns:env="http://www.w3.org/2003/05/soap-envelope"
              xmlns:m="http://www.example.org/timeouts"
              xmlns:xml="http://www.w3.org/XML/1998/namespace">
 <env:Body>
  <env:Fault>
   <env:Code>
     <env:Value>env:Sender</env:Value>
     <env:Subcode>
      <env:Value>m:MessageTimeout</env:Value>
     </env:Subcode>
   </env:Code>
  
   <env:Reason>
     <env:Text xml:lang="en">Sender Timeout</env:Text>
      <env:Text xml:lang="af">Besender tuid</env:Text>
   </env:Reason>
   
    <env:Node>http://jenkov.com/theNodeThatFailed</env:Node>

  <env:Role>http://www.w3.org/2003/05/soap-envelope/role/ultimateReceiver</env:Role>
   
   <env:Detail>detail string</env:Detail>    
  </env:Fault>
 </env:Body>
</env:Envelope>
"""		

 /**
  * SOAP fault w/o a Detail entry
  *
  * Unit references the fact that the type of the Fault instance
  * will be Unit (since there is no application specific value)
  * 
  */  
  val inFaultUnit = """<env:Envelope xmlns:env="http://www.w3.org/2003/05/soap-envelope"
              xmlns:m="http://www.example.org/timeouts"
              xmlns:xml="http://www.w3.org/XML/1998/namespace">
 <env:Body>
  <env:Fault>
   <env:Code>
     <env:Value>Sender</env:Value>
     <env:Subcode>
      <env:Value>m:MessageTimeout</env:Value>
     </env:Subcode>
   </env:Code>
  
   <env:Reason>
     <env:Text xml:lang="en">Sender Timeout</env:Text>
      <env:Text xml:lang="af">Besender tuid</env:Text>
   </env:Reason>  
  </env:Fault>
 </env:Body>
</env:Envelope>
"""		
  //The application specific SOAP fault Detail 
  case class Timeouts(issue:String,maxTime:String)
    
  //pickler
  object Timeouts{
      import Picklers._
      
      implicit val MURI = URI("http://www.example.org/timeouts")
      val SOAP = URI(javax.xml.soap.SOAPConstants.URI_NS_SOAP_1_2_ENVELOPE)
      
      def rawPickler = attr(MURI,"issue",text) ~ elem("MaxTime",text)
      
      def pickler = wrapCaseClass(rawPickler)(Timeouts.apply)(Timeouts.unapply)
  }  
 
  //Faul with details
  val pFault = Fault[Timeouts](Sender,Some("m:MessageTimeout"),List("Sender Timeout","Besender tuid"),Some("http://jenkov.com/theNodeThatFailed"),Some("http://www.w3.org/2003/05/soap-envelope/role/ultimateReceiver"),
      Some(Timeouts("timing","P5M"))
  )

   //Fault with simple String for details
  val pFaultString = Fault[String](Sender,Some("m:MessageTimeout"),List("Sender Timeout","Besender tuid"),Some("http://jenkov.com/theNodeThatFailed"),Some("http://www.w3.org/2003/05/soap-envelope/role/ultimateReceiver"),
      Some("detail string")
  )
 
  //Fault w/o details 
  val pFaultUnit = Fault[Unit](Sender,Some("m:MessageTimeout"),List("Sender Timeout","Besender tuid"),None,None,None)
  
   "parseFaultUnit" in {
      val result = Fault.pickler(null).unpickle(inFaultUnit)
     
      result match {
         case Success(v, _) => pFaultUnit must beEqualTo(v)
         case f: NoSuccess  => fail(f toString)
    }
}      
  
   "parseFault" in {
      val result = Fault.pickler(Timeouts.pickler).unpickle(inFault)
     
      result match {
         case Success(v, _) => pFault must beEqualTo(v)
         case f: NoSuccess  => fail(f toString)
      }
   }      

   "parseFaultString" in {
      val result = Fault.pickler(text).unpickle(inFaultString)
     
      result match {
         case Success(v, _) => pFaultString must beEqualTo(v)
         case f: NoSuccess  => fail(f toString)
      }
   }      


  "unparseFault" in { 
    val xml =   Fault.pickler(Timeouts.pickler).pickle(pFault)
    """<Envelope xmlns="http://www.w3.org/2003/05/soap-envelope">
<Body>
<Fault>
<Code>
<Value>Sender</Value>
<Subcode>
<Value>m:MessageTimeout</Value>
</Subcode>
</Code>
<Reason>
<Text>Sender Timeout</Text>
<Text>Besender tuid</Text>
</Reason>
<Node>http://jenkov.com/theNodeThatFailed</Node>
<Role>http://www.w3.org/2003/05/soap-envelope/role/ultimateReceiver</Role>
<Detail xmlns:ns0="http://www.example.org/timeouts" ns0:issue="timing">
<MaxTime xmlns="http://www.example.org/timeouts">P5M</MaxTime>
</Detail>
</Fault>
</Body>
</Envelope>
""" must beEqualTo(normalize(xml))
  }

  "unparseFaultUnit" in {
    val xml=   Fault.pickler(null).pickle(pFaultUnit)
    """<Envelope xmlns="http://www.w3.org/2003/05/soap-envelope">
<Body>
<Fault>
<Code>
<Value>Sender</Value>
<Subcode>
<Value>m:MessageTimeout</Value>
</Subcode>
</Code>
<Reason>
<Text>Sender Timeout</Text>
<Text>Besender tuid</Text>
</Reason>
</Fault>
</Body>
</Envelope>
""" must beEqualTo(normalize(xml))
  }

  

   "parseInternal" in {
     val in = """<env:Envelope xmlns:env="http://www.w3.org/2003/05/soap-envelope">
                   <env:Body> 
                    <internal xmlns="nested-uri">
                       <tag>tagged</tag>
                       <value>123</value>
                    </internal>
                   </env:Body>
                 </env:Envelope>"""
            
     val result = DocLiteral.pickler(Internal.internalPickler).unpickle(in)
     
     result match {
      case Success(v, _) => DocLiteral(Internal("tagged",123)) must beEqualTo(v)
      case f: NoSuccess  => fail(f toString)
    }
 }


 "parseContained" in {
     val in = """<env:Envelope xmlns:env="http://www.w3.org/2003/05/soap-envelope">
                   <env:Body> 
                       <tag xmlns="contained-uri">tagged</tag>
                       <value xmlns="contained-uri">123</value>
                  </env:Body>
                 </env:Envelope>"""
            
     val result = DocLiteral.pickler(Contained.pickler).unpickle(in)
     
     result match {
        case Success(v, _) => DocLiteral(Contained("tagged",123)) must beEqualTo(v)
        case f: NoSuccess  => fail(f toString)
     }
  }
 
   //structured body with a container element
   "unparseInternal" in {
      val r=  DocLiteral(Internal("tagged",123))
       
      val xml=   DocLiteral.pickler(Internal.internalPickler).pickle(r)
    """<Envelope xmlns="http://www.w3.org/2003/05/soap-envelope">
<Body>
<internal xmlns="nested-uri">
<tag>tagged</tag>
<value>123</value>
</internal>
</Body>
</Envelope>
""" must beEqualTo(normalize(xml))

     checkBody( "\n\ntagged\n123\n\n",xml.document)
  }

   //Structured body w/o container element
   "unparseContained" in {
     val r =  DocLiteral(Contained("tagged",123))
     val out = PlainOutputStore.empty
     val xml =   DocLiteral.pickler(Contained.pickler).pickle(r,out)
    """<Envelope xmlns="http://www.w3.org/2003/05/soap-envelope">
<Body>
<tag xmlns="contained-uri">tagged</tag>
<value xmlns="contained-uri">123</value>
</Body>
</Envelope>
""" must beEqualTo(normalize(xml.document)) 

     checkBody( "\ntagged\n123\n",xml.document)
   }

  //DocLiteral of a bare string 
  "unparseString" in {
     val r =  DocLiteral("content")
     val out = PlainOutputStore.empty
     val xml =   DocLiteral.pickler(text).pickle(r,out)
    """<Envelope xmlns="http://www.w3.org/2003/05/soap-envelope">
<Body>content</Body>
</Envelope>
""" must beEqualTo(normalize(xml.document)) 

     checkBody( "content",xml.document)
   } 

   //DocLiteral of a bare integer
   "unparseInt" in {
     val r =  DocLiteral(123)
     val out = PlainOutputStore.empty
     val xml =   DocLiteral.pickler(intVal).pickle(r,out)
    """<Envelope xmlns="http://www.w3.org/2003/05/soap-envelope">
<Body>123</Body>
</Envelope>
""" must beEqualTo(normalize(xml.document)) 

     checkBody( "123",xml.document)
   } 

  "faultCodeConvertGood" in {
     import FaultCodeConvert._
     Sender  must beEqualTo(parse("env:Sender"))
     Sender  must beEqualTo(parse("x:Sender"))
     Sender  must beEqualTo(parse(":Sender"))
     Sender  must beEqualTo(parse(":Sender"))
     Receiver  must beEqualTo(parse("env:Receiver"))
  }


   "faultCodeConvertBadCode" in {
      import FaultCodeConvert._
      parse("env:xyz") must throwA(new scala.MatchError("xyz"))
   }

   /**
    * Use standard Java SOAP implementation to ensure Scala code 
    * correctly serializes the XML representation
    */
   private def checkBody(expected:String,xml:org.w3c.dom.Document){
     import javax.xml.soap._
     import java.io._

     val factory = MessageFactory.newInstance(SOAPConstants.SOAP_1_2_PROTOCOL)
     val message = factory.createMessage(new MimeHeaders(),new ByteArrayInputStream(normalize(xml).getBytes))
     expected must beEqualTo(message.getSOAPBody.getTextContent)
   }
      
}
