/* Copyright (c) 2008 Google Inc.
 * Copyright (c) 2010 Richard Searle
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

/*
  Extracted from gdata-scala
  Modified by Richard Searle
*/

package cognitiveentity.xml.combinators

/**
 * Mix in this trait to get implicit conversions between functions of arity 2-7 and 
 * functions that take nested instances of '~' pairs.
 * 
 * This is particularly useful when pickling case classes. Complier generated functions for
 * apply and unapply are then implicitly converted to functions that take '~' instances.
 * 
 * Example:
 * <code>
 * case class Foo(x: Int, y: String)
 * 
 * wrap (elem("x", intVal) ~ elem("y", text)) Foo.apply Foo.unapply
 * 
 * @author Iulian Dragos
 * @author Richard Searle
 */
trait TupleToPairFunctions {
  /** Convert a binary function to a function of a pair. */
  implicit def fun2ToPair[A, B, C](fun: (A, B) => C): (A ~ B) => C = { 
    case a ~ b => fun(a, b)
  }
    
  /** Convert a function of 3 arguments to one that takes a pair of a pair. */
  implicit def fun3ToPpairL[A, B, C, D](fun: (A, B, C) => D): (A ~ B ~ C) => D = { 
    case a ~ b ~ c =>  fun(a, b, c)
  }
    
  /** Convert a function of 4 arguments to one that takes a pair of a pair. */
  implicit def fun4ToPpairL[A, B, C, D, E]
      (fun: (A, B, C, D) => E): A ~ B ~ C ~ D => E = { 
    case a ~ b ~ c ~ d =>  fun(a, b, c, d)
  }

  /** Convert a function of 5 arguments to one that takes a pair of a pair. */
  implicit def fun5ToPpairL[A, B, C, D, E, F]
      (fun: (A, B, C, D, E) => F): (A ~ B ~ C ~ D ~ E) => F = { 
    case a ~ b ~ c ~ d ~ e =>  fun(a, b, c, d, e)
  }

  /** Convert a function of 6 arguments to one that takes a pair of a pair. */
  implicit def fun6ToPpairL[A, B, C, D, E, F, G]
      (fun: (A, B, C, D, E, F) => G): (A ~ B ~ C ~ D ~ E ~ F) => G = { 
    case a ~ b ~ c ~ d ~ e ~ f =>  fun(a, b, c, d, e, f)
  }

  /** Convert a function of 7 arguments to one that takes a pair of a pair. */
  implicit def fun7ToPpairL[A, B, C, D, E, F, G, H]
      (fun: (A, B, C, D, E, F, G) => H): (A ~ B ~ C ~ D ~ E ~ F ~ G) => H = { 
    case a ~ b ~ c ~ d ~ e ~ f ~ g =>  fun(a, b, c, d, e, f, g)
  }

   implicit def bareUnapply[ B](a: Option[B]) = 
    Some(a.get)
  
  implicit def tuple2Unapply[ B, C](a: Option[(B, C)]) = 
    Some(new ~(a.get._1, a.get._2))
  
  implicit def tuple3Unapply[ B, C, D](a: Option[(B, C,D)]) = 
    Some(new ~(a.get._1, a.get._2) ~a.get._3)
   
  implicit def tuple4Unapply[ A,B, C, D](a: Option[(A, B, C, D)]) = 
    Some(new ~(a.get._1, a.get._2) ~ a.get._3 ~ a.get._4)
    
  implicit def tuple5Unapply[ A, B, C, D, E](a: Option[(A, B, C, D, E)]) = 
    Some(new ~(a.get._1, a.get._2) ~ a.get._3 ~ a.get._4 ~ a.get._5)

  implicit def tuple6Unapply[ A, B, C, D, E, F](a: Option[(A, B, C, D, E, F)]) = 
    Some(new ~(a.get._1, a.get._2) ~ a.get._3 ~ a.get._4 ~ a.get._5 ~ a.get._6)
  
  
}
