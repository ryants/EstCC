name := "EstCC"

scalaVersion := "2.11.3"

scalacOptions += "-feature"

libraryDependencies += "colt" % "colt" % "1.2.0"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.1.5" % "test"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.2.0"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.3"

resolvers += Resolver.sonatypeRepo("public")

exportJars := true

seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

