name := "jsonParser"

version := "1.0"

scalaVersion := "2.12.1"

scalacOptions in Compile := (scalacOptions in Compile).value.filter(_ != "-Yinline-warnings")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"