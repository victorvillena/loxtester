// Autoreload the build whenever it is changed
Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.6.3"

lazy val root = (project in file("."))
  .settings(
    name             := "loxtester",
    idePackagePrefix := Some("org.willena.loxtester"),
    libraryDependencies := Seq(
      "org.scalameta" %% "munit"  % "1.0.2"  % Test,
      "com.lihaoyi"   %% "os-lib" % "0.10.7",
    ),
  )
