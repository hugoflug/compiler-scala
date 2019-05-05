name := "compiler-scala"

scalaVersion := "2.12.8"

libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.1.0"
libraryDependencies += "org.scalatest" % "scalatest_2.12" % "3.0.5" % "test"
libraryDependencies += "co.fs2" %% "fs2-core" % "1.0.4"
libraryDependencies += "co.fs2" %% "fs2-io" % "1.0.4"

logBuffered in Test := false
assemblyJarName in assembly := "mjc.jar"