package com.google.xml.combinators


case class SoapDocLiteral[T](contents:T)

object SoapDocLiteral {

  import Picklers._

  final val SOAP = URI(javax.xml.soap.SOAPConstants.URI_NS_SOAP_1_2_ENVELOPE)
   
  def rawPickler[T](cp:Pickler[T])  = elem(SOAP , "Envelope", elem(SOAP, "Body", cp))
  
   private def fromSoapDocLiteral[T](n:SoapDocLiteral[T]) = Some(SoapDocLiteral.unapply[T](n).get)

   def pickler[T](cp:Pickler[T]): Pickler[SoapDocLiteral[T]] = wrapCaseClass(rawPickler(cp)) (SoapDocLiteral.apply[T]) (fromSoapDocLiteral[T])

   
}