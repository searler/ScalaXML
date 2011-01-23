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

import org.specs._
import scala.xml.PrettyPrinter
import Picklers._


/**
 * @author Richard Searle
 *
 */
class MapContainedTest  extends PicklerAsserts{

   val in = """<container xmlns="testing-uri">
<name>name</name>
<key>tagged</key>
<value>123</value>
<key>next</key>
<value>1623</value>
</container>
"""

   val value = SimpleMapContainer("name",Map("tagged" -> MapContained("tagged",123),
                                             "next" -> MapContained("next",1623)))
   
   "parseContained" in {         
     val result = SimpleMapContainer.pickler.unpickle(LinearStore(in))  
     result match {
         case Success(v, _) => value must beEqualTo(v)
         case f: NoSuccess  => fail(f toString)
     }
   }
 
   "unparseContained" in { 
    val out = PlainOutputStore.empty
    val xml = SimpleMapContainer.pickler.pickle(value,out)
    in must beEqualTo(normalize(xml.document))
  }
      
}
