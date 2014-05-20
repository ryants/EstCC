name := "EstCC"

scalaVersion := "2.10.2"

scalacOptions += "-feature"

libraryDependencies += "colt" % "colt" % "1.2.0"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0" % "test"

exportJars := true

seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

