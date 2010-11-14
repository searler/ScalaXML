package com.google.xml.combinators


case class DocLiteral[T](contents:T)

object DocLiteral {

  import Picklers._

  final val SOAP = URI(javax.xml.soap.SOAPConstants.URI_NS_SOAP_1_2_ENVELOPE)
   
  def rawPickler[T](cp:Pickler[T])  = elem(SOAP , "Envelope", elem(SOAP, "Body", cp))
  
   private def fromDocLiteral[T](n:DocLiteral[T]) = Some(DocLiteral.unapply[T](n).get)

   def pickler[T](cp:Pickler[T]): Pickler[DocLiteral[T]] = wrapCaseClass(rawPickler(cp)) (DocLiteral.apply[T]) (fromDocLiteral[T])

   
}