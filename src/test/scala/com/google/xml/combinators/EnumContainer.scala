package com.google.xml.combinators



case class  EnumContainer(s:String,e:TestEnum)

object EnumContainer{
 import Picklers._

 implicit object EnumConverter extends EnumConvert(classOf[TestEnum])

  final val TURI = URI("testing-uri")
  def rawPickler  = 
    elem("container", 
      elem("name",text)(TURI)  ~ elem("enum", typedValue)(TURI)
        )(TURI)
  
   

def pickler: Pickler[EnumContainer] = wrapCaseClass(rawPickler) (EnumContainer.apply) (EnumContainer.unapply)

}