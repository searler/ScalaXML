package com.google.xml.combinators


case class SoapDocLiteral[T](contents:T)

object SoapDocLiteral {

  import Picklers._

  final val SOAP = URI("http://www.w3.org/2003/05/soap-envelope")
   
  def rawPickler[T](cp:Pickler[T])  = 
    elem(SOAP , "Envelope", 
      elem(SOAP, "Body", cp)  
        )
  
   private def fromSoapDocLiteral[T](n:SoapDocLiteral[T]) = Some(SoapDocLiteral.unapply[T](n).get)

   def pickler[T](cp:Pickler[T]): Pickler[SoapDocLiteral[T]] = wrapCaseClass(rawPickler(cp)) (SoapDocLiteral.apply[T]) (fromSoapDocLiteral[T])

   
}