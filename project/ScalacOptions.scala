object ScalacOptions {
  final val standards = Seq(
    "-deprecation",
    "-encoding", "utf-8",
    "-explaintypes", 
    "-feature",
    "-language:existentials",
    "-language:experimental.macros",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-unchecked" 
  )

  final val advanced = Seq(
    "-Xcheckinit", 
    "-Xfatal-warnings", 
    "-Xfuture" 
  )

  final val warnings = Seq(
    "-Xlint:adapted-args", 
    "-Xlint:by-name-right-associative", 
    "-Xlint:constant", 
    "-Xlint:delayedinit-select", 
    "-Xlint:doc-detached", 
    "-Xlint:inaccessible", 
    "-Xlint:infer-any", 
    "-Xlint:missing-interpolator", 
    "-Xlint:nullary-override", 
    "-Xlint:nullary-unit", 
    "-Xlint:option-implicit", 
    "-Xlint:package-object-classes", 
    "-Xlint:poly-implicit-overload", 
    "-Xlint:private-shadow", 
    "-Xlint:stars-align", 
    "-Xlint:type-parameter-shadow", 
    "-Xlint:unsound-match", 
    "-Yno-adapted-args", 
    "-Ypartial-unification", 
    "-Ywarn-dead-code", 
    "-Ywarn-extra-implicit", 
    "-Ywarn-inaccessible", 
    "-Ywarn-infer-any", 
    "-Ywarn-nullary-override", 
    "-Ywarn-nullary-unit", 
    "-Ywarn-numeric-widen", 
    "-Ywarn-unused:implicits", 
    "-Ywarn-unused:imports", 
    "-Ywarn-unused:locals", 
    "-Ywarn-unused:params", 
    "-Ywarn-unused:patvars", 
    "-Ywarn-unused:privates", 
    "-Ywarn-value-discard" 
  )
}
