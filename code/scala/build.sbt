name := """lazr"""

version := "1.0"

scalaVersion := "2.10.6"

mainClass in Compile := Some("Launcher")

// Change this to another test framework if you prefer

libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "2.2.4" % "test"
  , "io.spray"  %% "spray-can"     % "1.3.3"
  , "io.spray"  %% "spray-httpx"   % "1.3.3"
  , "io.spray"  %% "spray-routing-shapeless2" % "1.3.3"
  , "com.typesafe.akka" %% "akka-actor" % "2.3.6"
  , "com.typesafe.akka" %% "akka-slf4j" % "2.3.6"
  , "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.3"
  , "org.parboiled" %% "parboiled" % "2.1.0"
)

Revolver.settings