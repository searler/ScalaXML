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
case class Fault(code:String,subCode:Option[String],reason:List[String])

object Fault{
 import Picklers._
   implicit val SOAP = URI(javax.xml.soap.SOAPConstants.URI_NS_SOAP_1_2_ENVELOPE)

    def rawPickler = elem( "Envelope", elem( "Body",elem("Fault",
                elem("Code",elem("Value",text) ~ opt(elem("Subcode",elem("Value",text)))) ~
 elem("Reason", list(elem("Text",text))))
     ))

    def pickler() = wrapCaseClass(rawPickler) (Fault.apply) (Fault.unapply)
}