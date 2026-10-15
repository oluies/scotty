val scala3Version = "3.9.0" // LTS
val scalaTestVersion = "3.2.20"
val parallelCollectionsVersion = "1.2.0"

ThisBuild / organization := "xyz.entangled"
ThisBuild / version := "0.7.0-SNAPSHOT"
ThisBuild / scalaVersion := scala3Version
ThisBuild / versionScheme := Some("early-semver")

lazy val root = (project in file("."))
  .settings(
    name := "scotty",
    description := "Quantum computing in Scala: state-vector simulator plus a density-matrix and quantum-channel layer.",
    homepage := Some(url("https://github.com/entangled-xyz/scotty")),
    licenses := Seq("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.txt")),
    developers := List(
      Developer(
        id = "vasinov",
        name = "Vasily Vasinov",
        email = "vasinov@me.com",
        url = url("https://www.vasinov.com")
      )
    ),
    // Built and tested on the latest JDK (27 at the time of writing); `-release` keeps the emitted
    // bytecode and the visible JDK API at JDK 21 so the library stays usable there. Scala 3.9
    // accepts `-release` up to 25, and itself needs JDK 17 or newer to run.
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked",
      "-explain-types",
      "-release:21"
    ),
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parallel-collections" % parallelCollectionsVersion,
      "org.scalactic" %% "scalactic" % scalaTestVersion % Test,
      "org.scalatest" %% "scalatest" % scalaTestVersion % Test
    ),
    publishMavenStyle := true,
    publishTo := Some(Resolver.defaultLocal)
  )
