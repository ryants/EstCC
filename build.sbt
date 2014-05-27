name := "EstCC"

scalaVersion := "2.11.0"

scalacOptions += "-feature"

libraryDependencies += "colt" % "colt" % "1.2.0"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0" % "test"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.2.0"

resolvers += Resolver.sonatypeRepo("public")

exportJars := true

seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

