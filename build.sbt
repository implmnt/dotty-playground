val dottyVersion = dottyLatestNightlyBuild.get

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-playground",
    version := "0.1.0",

    scalaVersion := dottyVersion
  )
