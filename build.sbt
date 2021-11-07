name := "excel-parser"

scalaVersion       := "2.13.7"

scalacOptions     ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:higherKinds",
  "-language:implicitConversions", "-language:existentials",
  "-unchecked",
//  "-Xfatal-warnings",
  "-Xlint",
//  "-Yno-adapted-args",
  //    "-Ywarn-numeric-widen",
  "-Ywarn-value-discard"
//  "-Xfuture"
)

resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies ++= Seq(
  "org.typelevel"  %% "cats-core"  % "2.6.1",
  "org.apache.poi" % "poi-ooxml"   % "5.1.0",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "org.apache.logging.log4j" % "log4j-slf4j-impl" % "2.14.1",
  "org.scalatest"  %% "scalatest"  % "3.2.10"  % "test"
)

scalafmtOnCompile := true

addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full)

enablePlugins(ScalafmtPlugin, MdocPlugin)

Global / onChangedBuildSource := ReloadOnSourceChanges
