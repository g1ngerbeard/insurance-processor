import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.coya",
      scalaVersion := "2.12.7",
      version      := "0.1.1-SNAPSHOT"
    )),
    name := "CoyaBackendChallenge",
    libraryDependencies += scalaTest % Test
  )
