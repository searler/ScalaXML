import sbt._

class GDataScalaProject(info: ProjectInfo) extends DefaultProject(info)
{
 val specs = "org.scala-tools.testing" % "specs_2.8.1" % "1.6.6"

 override def compileOrder = CompileOrder.JavaThenScala

}
