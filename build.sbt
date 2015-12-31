name := "EstCC"

scalaVersion := "2.11.7"

scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked")

libraryDependencies ++= Seq("colt" % "colt" % "1.2.0", 
"org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
"com.github.scopt" %% "scopt" % "3.2.0",
"com.typesafe.akka" %% "akka-actor" % "2.3.3")

resolvers ++= Seq(Resolver.sonatypeRepo("public"),
"Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/releases")

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")

exportJars := true

seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

parallelExecution in Test := false

testOptions in Test := Seq(Tests.Filter(s => s.endsWith("Test")))
