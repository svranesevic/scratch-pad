scalaVersion := "2.13.8"
scalacOptions += "-language:higherKinds"
addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.13.2" cross CrossVersion.full)

libraryDependencies ++= Seq(
  "dev.zio" %% "zio-prelude" % "1.0.0-RC13",
  "dev.zio" %% "zio" % "2.0.0-RC5"
)
scalacOptions += "-Wconf:cat=other-match-analysis:error"
