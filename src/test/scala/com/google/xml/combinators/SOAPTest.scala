package com.google.xml.combinators

import org.specs._

import scala.xml.PrettyPrinter

import Picklers._

class SOAPTest  extends PicklerAsserts{

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
 
  case class Timeouts(issue:String,maxTime:String)
    
  object Timeouts{
      import Picklers._
      
      implicit val MURI = URI("http://www.example.org/timeouts")
      val SOAP = URI(javax.xml.soap.SOAPConstants.URI_NS_SOAP_1_2_ENVELOPE)
      
      def rawPickler = attr(MURI,"issue",text) ~ elem("MaxTime",text)
      
      def pickler = wrapCaseClass(rawPickler)(Timeouts.apply)(Timeouts.unapply)
  }  
 
  val pFault = Fault[Timeouts](Sender,Some("m:MessageTimeout"),List("Sender Timeout","Besender tuid"),Some("http://jenkov.com/theNodeThatFailed"),Some("http://www.w3.org/2003/05/soap-envelope/role/ultimateReceiver"),
      Some(Timeouts("timing","P5M"))
  )
  
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

 "unparseFault" in {
   
    val xml=   Fault.pickler(Timeouts.pickler).pickle(pFault)
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

  def checkBody(expected:String,xml:org.w3c.dom.Document){
     import javax.xml.soap._
     import java.io._

     val factory = MessageFactory.newInstance(SOAPConstants.SOAP_1_2_PROTOCOL)
     val message = factory.createMessage(new MimeHeaders(),new ByteArrayInputStream(normalize(xml).getBytes))
     expected must beEqualTo(message.getSOAPBody.getTextContent)
  }
      
}
