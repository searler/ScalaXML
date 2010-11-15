package com.google.xml.combinators


case class DocLiteral[T](contents:T)
 

object DocLiteral {

  import Picklers._
  val SOAP = URI(javax.xml.soap.SOAPConstants.URI_NS_SOAP_1_2_ENVELOPE)

   
  def rawPickler[T](cp:Pickler[T])  = elem(SOAP , "Envelope", elem(SOAP, "Body", cp))
  
   private def fromDocLiteral[T](n:DocLiteral[T]) = Some(DocLiteral.unapply[T](n).get)

   def pickler[T](cp:Pickler[T]): Pickler[DocLiteral[T]] = wrapCaseClass(rawPickler(cp)) (DocLiteral.apply[T]) (fromDocLiteral[T])
}

//ignore language code

trait FaultCode
case object VersionMismatch extends FaultCode
case object MustUnderstand extends FaultCode
case object DataEncodingUnknown extends FaultCode
case object Sender extends FaultCode
case object Receiver extends FaultCode

 object  FaultCodeConvert extends Convert[FaultCode] {   
    def parse(s:String) =  s match {
      case "env:Sender" =>Sender
    }
    def unparse(v:FaultCode) = v toString
   }

case class Fault[T](code:FaultCode,subCode:Option[String],reason:List[String],node:Option[String],role:Option[String],details:Option[T])

object Fault{
 import Picklers._
   implicit val SOAP = URI(javax.xml.soap.SOAPConstants.URI_NS_SOAP_1_2_ENVELOPE)

    def rawPickler[T](dp:Pickler[T]) = elem( "Envelope", elem( "Body",elem("Fault",
                elem("Code",elem("Value",typedValue(FaultCodeConvert)) ~ opt(elem("Subcode",elem("Value",text)))) ~
                elem("Reason", list(elem("Text",text))) ~
                opt(elem("Node", text)) ~
                opt(elem("Role",text)) ~
                opt(elem(SOAP,"Detail",dp)) 
           )
     ))

   private def fromDetail[T](n:Fault[T]) = tuple6Unapply(Fault.unapply[T](n))

    def pickler[T](dp:Pickler[T]):Pickler[Fault[T]] = wrapCaseClass(rawPickler(dp)) (Fault.apply[T]) (fromDetail[T])
}