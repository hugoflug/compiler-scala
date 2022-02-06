name := "compiler-scala"

scalaVersion := "2.12.12"

libraryDependencies += "org.typelevel" %% "cats-parse" % "0.3.6"
libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.1.0"
libraryDependencies += "org.scalatest" % "scalatest_2.12" % "3.0.5" % "test"

logBuffered in Test := false
assemblyJarName in assembly := "mjc.jar"