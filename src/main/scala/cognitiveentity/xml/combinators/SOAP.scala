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

/**
 * @author Richard Searle
 * 
 * This set of classes and picklers supports SOAP 1.2 doc literal exchanges. The implementation
 * is incomplete (e.g. i18n is ignored) and does not enforce all SOAP 1.2 requirements (e.g.
 * URI for fault codes is ignored)
 *
 * The goal is to provide a simple way to exchange XML payloads and faults over a SOAP
 * compliant interface.
 */

/**
 * Represents a DocLiteral encoding of the contents
 */
case class DocLiteral[T](contents:T)
 
/**
 * Pickler for DocLiteral
 */
object DocLiteral {
   import Picklers._
   val SOAP = URI(javax.xml.soap.SOAPConstants.URI_NS_SOAP_1_2_ENVELOPE)
   
   def rawPickler[T](cp:Pickler[T])  = elem(SOAP , "Envelope", elem(SOAP, "Body", cp))
  
   private def fromDocLiteral[T](n:DocLiteral[T]) = Some(DocLiteral.unapply[T](n).get)

   def pickler[T](cp:Pickler[T]): Pickler[DocLiteral[T]] = wrapCaseClass(rawPickler(cp)) (DocLiteral.apply[T]) (fromDocLiteral[T])
}



/**
 * Represent SOAP 1.2 fault codes
 */
sealed trait FaultCode
case object VersionMismatch extends FaultCode
case object MustUnderstand extends FaultCode
case object DataEncodingUnknown extends FaultCode
case object Sender extends FaultCode
case object Receiver extends FaultCode

/**
 * Converts FaultCodes to/from the string representation
 * Prefix (if any) is ignored
 */
object  FaultCodeConvert extends Convert[FaultCode] {   
    def parse(s:String) =  {
       //do not care about prefix
       val pieces = s.split(":")
       pieces(pieces.length -1)  match {
          case "Sender" => Sender
          case "Receiver" => Receiver
          case "DataEncodingUnknown" => DataEncodingUnknown
          case "MustUnderstand" => MustUnderstand
          case "VersionMismatch" => VersionMismatch
        }
    }
    def unparse(v:FaultCode) = v toString
   }

/**
 * Represents a SOAP fault, optionally containing application defined details.
 */
case class Fault[T](code:FaultCode,subCode:Option[String],reason:List[String],node:Option[String],role:Option[String],details:Option[T])

/**
 * Fault pickler
 */
object Fault{
   import Picklers._
   implicit val SOAP = URI(javax.xml.soap.SOAPConstants.URI_NS_SOAP_1_2_ENVELOPE)

   def rawPickler[T](dp:Pickler[T]) = elem( "Envelope", elem( "Body",elem("Fault",
                elem("Code",elem("Value",typedValue(FaultCodeConvert)) ~ opt(elem("Subcode",elem("Value",text)))) ~ //ignore recursion
                elem("Reason", list(elem("Text",text))) ~
                opt(elem("Node", text)) ~
                opt(elem("Role",text)) ~
                opt(elem(SOAP,"Detail",dp)) 
           )
   ))

   private def fromDetail[T](n:Fault[T]) = tuple6Unapply(Fault.unapply[T](n))

   def pickler[T](dp:Pickler[T]):Pickler[Fault[T]] = wrapCaseClass(rawPickler(dp)) (Fault.apply[T]) (fromDetail[T])
}