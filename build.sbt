name := "tpattern"

version := "1.0"

scalaVersion := "2.12.6"

libraryDependencies ++= Seq(
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0",
  "com.github.nscala-time" %% "nscala-time" % "2.18.0",
  "org.scalanlp" %% "breeze" % "0.13.2"
)