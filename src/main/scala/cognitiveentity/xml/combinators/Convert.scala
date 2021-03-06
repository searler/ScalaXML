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

/**
 * @author Richard Searle
 */
package cognitiveentity.xml.combinators

/**
 * Define the standard pickler interface
 */
trait Convert[T] {
  def parse(str:String):T
  def unparse(v:T):String
}

/**
 * Pickler for Java enumeration
 */
class EnumConvert[E <: Enum[E]](c:Class[E]) extends Convert[E]{
   def parse(s:String) =  Enum.valueOf(c,s).asInstanceOf[E]
   def unparse(v:E) = v name
}

/**
 * Define a collection of standard picklers, as implicits
 * so they are auto-magically used where necessary.
 *
 * It is sufficient to use typedValue in the parser definition, and
 * the appropriate converter is used based on the type defined in
 * case class. This avoids redundantly specifying the type in
 * two places.
 */
object Converters{

  implicit object  StringConvert extends Convert[String] {   
    def parse(s:String) =  s
    def unparse(v:String) = v
   }

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

  
}