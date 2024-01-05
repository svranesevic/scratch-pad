ThisBuild / organization := "io.svranesevic"
ThisBuild / version      := "0.1.0-SNAPSHOT"

lazy val commonDependencies = Seq(
  "dev.zio"       %% "zio"         % "2.0.13",
  "org.typelevel" %% "cats-effect" % "3.4.1"
)

lazy val `scratch-pad-2` = (project in file("modules/scratch-pad-2"))
  .settings(
    scalaVersion := "2.13.10",
    libraryDependencies ++= commonDependencies,
  )
  .enablePlugins(JmhPlugin)

lazy val `scratch-pad-3` = (project in file("modules/scratch-pad-3"))
  .settings(
    scalaVersion := "3.3.1",
    libraryDependencies ++= commonDependencies,
  )
  .enablePlugins(JmhPlugin)
