val scala3Version = "3.9.0" // LTS
val scalaTestVersion = "3.2.20"
val parallelCollectionsVersion = "1.2.0"

// The fork publishes under its own namespace: `xyz.entangled` on Maven Central belongs to the
// original project, whose last release there is 0.6.0. Releases go to GitHub Packages, from a `v*`
// tag through .github/workflows/release.yml.
ThisBuild / organization := "io.github.oluies"
ThisBuild / version := "0.7.2-SNAPSHOT"
ThisBuild / scalaVersion := scala3Version
ThisBuild / versionScheme := Some("early-semver")

lazy val root = (project in file("."))
  .settings(
    name := "scotty",
    description := "Quantum computing in Scala: state-vector simulator plus a density-matrix and quantum-channel layer.",
    homepage := Some(url("https://github.com/oluies/scotty")),
    scmInfo := Some(ScmInfo(url("https://github.com/oluies/scotty"), "scm:git:https://github.com/oluies/scotty.git")),
    licenses := Seq("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.txt")),
    developers := List(
      Developer(
        id = "vasinov",
        name = "Vasily Vasinov",
        email = "vasinov@me.com",
        url = url("https://www.vasinov.com")
      ),
      Developer(
        id = "oluies",
        name = "Örjan Lundberg",
        email = "orjan.lundberg@gmail.com",
        url = url("https://github.com/oluies")
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
    // `publishLocal` and `publishM2` are unaffected; `publish` goes to GitHub Packages, with the
    // workflow's GITHUB_TOKEN when it is set.
    publishTo := Some("GitHub Packages" at "https://maven.pkg.github.com/oluies/scotty"),
    credentials ++= sys.env
      .get("GITHUB_TOKEN")
      .map(token => Credentials("GitHub Package Registry", "maven.pkg.github.com", sys.env.getOrElse("GITHUB_ACTOR", "oluies"), token))
      .toSeq
  )
