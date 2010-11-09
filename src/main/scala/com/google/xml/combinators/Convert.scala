package com.google.xml.combinators

trait Convert[T] {
  def parse(str:String):T
  def unparse(v:T):String
}

class EnumConvert[E <: Enum[E]](c:Class[E]) extends Convert[E]{
   def parse(s:String) =  Enum.valueOf(c,s).asInstanceOf[E]
   def unparse(v:E) = v toString
}

object Converters{

  implicit object  IntConvert extends Convert[Int] {   
    def parse(s:String) =  s toInt 
    def unparse(v:Int) = v toString
   }

   implicit object  FloatConvert extends Convert[Float] {   
    def parse(s:String) =  s toFloat 
    def unparse(v:Float) = v toString
   }

 implicit object  DoubleConvert extends Convert[Double] {   
    def parse(s:String) =  s toDouble
    def unparse(v:Double) = v toString
   }

  implicit object  BooleanConvert extends Convert[Boolean] {   
    def parse(s:String) =  s toBoolean
    def unparse(v:Boolean) = v toString
   }

    implicit object  StringConvert extends Convert[String] {   
    def parse(s:String) =  s
    def unparse(v:String) = v
   }

 
}