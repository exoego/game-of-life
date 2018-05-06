import sbt._

object Dependencies {
  final val tests = Seq(
    "org.scalatest"  %% "scalatest"  % "3.0.5"  % Test,
    "org.scalacheck" %% "scalacheck" % "1.14.0" % Test
  )
}