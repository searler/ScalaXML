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

import sbt._
import reaktor.scct.ScctProject

/**
 * @author Richard Searle
 */
class ScalaXMLProject(info: ProjectInfo) extends DefaultProject(info) with ScctProject
{
 val specs2 = "org.specs2" %% "specs2" % "1.4" % "test"
 val scalaz = "org.specs2" %% "specs2-scalaz-core" % "6.0.RC2" % "test"


 def specs2Framework = new TestFramework("org.specs2.runner.SpecsFramework")
 override def testFrameworks = super.testFrameworks ++ Seq(specs2Framework)

 val snapshots = "snapshots" at "http://scala-tools.org/repo-snapshots"
 val releases  = "releases" at "http://scala-tools.org/repo-releases"


 override def compileOrder = CompileOrder.JavaThenScala

}
