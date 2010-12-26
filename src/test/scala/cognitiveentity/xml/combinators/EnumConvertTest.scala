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
package com.google.xml.combinators

import org.specs._



import Picklers._

object EnumConvertTest extends PicklerAsserts{
  
  "unpickle" in {
    val r = new EnumContainer("name",TestEnum.BETA)
   val out = PlainOutputStore.empty
    val xml=   EnumContainer.pickler.pickle(r,out)
    """<container xmlns="testing-uri">
<name>name</name>
<enum>BETA</enum>
</container>
""" must beEqualTo(normalize(xml.document))

   }

"pickle" in {
    val in = """<container xmlns="testing-uri">
<name>name</name>
<enum>BETA</enum>
</container>"""
            
     val result = EnumContainer.pickler.unpickle(LinearStore(in))
     
      result match {
      case Success(v, _) => EnumContainer("name",TestEnum.BETA) must beEqualTo(v)
      case f: NoSuccess  => fail(f toString)
    }
}

}