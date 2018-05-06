name := """game-of-life"""

version := "1.0-SNAPSHOT"

organization := "net.exoego"

scalaVersion := "2.12.6"

libraryDependencies ++= Dependencies.processing ++ Dependencies.tests

scalafmtOnCompile := true

scalacOptions ++= ScalacOptions.standards ++ ScalacOptions.advanced ++ ScalacOptions.warnings

scalacOptions in (Compile, console) ~= (_.filterNot(
  Set(
    "-Ywarn-unused:imports",
    "-Xfatal-warnings"
  )))

scalacOptions in (Test, compile) ~= (_.filterNot(
  Set(
    "-Ywarn-unused:imports",
    "-Ywarn-unused:params",
    "-Xfatal-warnings"
  )))
