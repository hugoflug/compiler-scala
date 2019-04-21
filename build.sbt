name := "compiler-scala"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.1.0"
libraryDependencies += "org.scalatest" % "scalatest_2.12" % "3.0.5" % "test"
libraryDependencies += "com.github.nikita-volkov" % "sext" % "0.2.6" // for debug