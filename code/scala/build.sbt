name := """lazr"""

version := "1.0"

scalaVersion := "2.10.6"

// Change this to another test framework if you prefer

libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "2.2.4" % "test"
  , "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.3"
  , "org.parboiled" %% "parboiled" % "2.1.0"
)
