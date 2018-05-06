import sbt._

object Dependencies {
  final val processing = Seq(
    "org.processing" % "core" % "3.3.7"
  )

  final val tests = Seq(
    "org.scalatest"  %% "scalatest"  % "3.0.5"  % Test,
    "org.scalacheck" %% "scalacheck" % "1.14.0" % Test
  )
}