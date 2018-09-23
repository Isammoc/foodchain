name := """foodchain"""
organization := "net.isammoc"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.12.6"

libraryDependencies += guice
libraryDependencies += "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.2" % Test
libraryDependencies += "com.lightbend.akka" %% "akka-stream-alpakka-sse" % "0.20"

// Adds additional packages into Twirl
//TwirlKeys.templateImports += "net.isammoc.controllers._"

// Adds additional packages into conf/routes
// play.sbt.routes.RoutesKeys.routesImport += "net.isammoc.binders._"
