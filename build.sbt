// Default scala used for day-to-day dev / IDE
val devScala = "3.4.1"

// During IntelliJ import (VM option -DIDEA_IMPORT=1), expose only Scala 3 to the IDE.
// From the CLI (no IDEA_IMPORT prop), keep the full cross set for + / ++.
ThisBuild / scalaVersion := devScala
ThisBuild / crossScalaVersions := {
  if (sys.props.get("IDEA_IMPORT").contains("1"))
    Seq(devScala)
  else {
    Seq(devScala, "2.11.12", "2.12.20", "2.13.14")
  }
}

ThisBuild / organization := "com.github.jonathanvdc"
ThisBuild / version := {
  def gitDescribe: Option[String] =
    try {
      Some(sys.process.Process("git describe --tags --always --dirty").lineStream_!.head)
    } catch {
      case _: Exception => None
    }

  val version = gitDescribe match {
    case Some(raw) =>
      val stripped = raw.stripPrefix("v")
      if (stripped.matches("""\d+\.\d+\.\d+""")) stripped
      else s"$stripped-SNAPSHOT"
    case None =>
      "0.0.0-SNAPSHOT"
  }

  version
}

ThisBuild / versionScheme := Some("early-semver")

ThisBuild / publishTo := {
  val repo = "https://maven.pkg.github.com/jonathanvdc/foresight"
  Some("GitHub Packages" at repo)
}

ThisBuild / credentials += Credentials(
  "GitHub Package Registry",
  "maven.pkg.github.com",
  sys.env.getOrElse("GITHUB_ACTOR", ""),
  sys.env.getOrElse("GITHUB_TOKEN", "")
)

// Common settings for all Scala versions
lazy val commonScalaSettings = Seq(
  scalaVersion       := (ThisBuild / scalaVersion).value,
  crossScalaVersions := (ThisBuild / crossScalaVersions).value,

  // Version-specific support directories
  Compile / unmanagedSourceDirectories ++= {
    val base = (Compile / sourceDirectory).value
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 11)) | Some((2, 12)) => Seq(base / "scala-2.11")
      case Some((2, 13))                 => Seq(base / "scala-2.13+")
      case Some((3, _))                  => Seq(base / "scala-2.13+", base / "scala-3")
      case _                             => Nil
    }
  },

  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 11)) | Some((2, 12)) =>
        Seq("-unchecked", "-deprecation", "-feature", "-Xmax-classfile-name", "100")
      case Some((2, _)) =>
        Seq("-unchecked", "-deprecation", "-feature")
      case Some((3, 4)) =>
        Seq(
          "-unchecked",
          "-deprecation",
          "-feature",
          // Silence deprecated `with` type operator
          "-Wconf:msg=with as a type operator has been deprecated:silent",
          // Silence deprecated wildcard types (`_`)
          "-Wconf:msg=is deprecated for wildcard arguments:silent",
          // Silence deprecated vararg splices (`x: _*`)
          "-Wconf:msg=no longer supported:silent",
          // Silence warnings about specialization
          "-Wconf:msg=is more specialized than the right hand side:silent"
        )
      case Some((3, _)) =>
        Seq("-unchecked", "-deprecation", "-feature")
      case _ =>
        Seq.empty
    }
  },

  // Compile / doc / scalacOptions := Seq("-implicits")
)

lazy val foresight = (project in file("foresight"))
  .settings(
    name := "foresight",
    commonScalaSettings,

    // Dependencies
    libraryDependencies ++= Dependencies.libraryDependencies(scalaVersion.value),

    // Testing
    libraryDependencies ++= Dependencies.testDependencies(scalaVersion.value).map(_ % Test),
    Test / logBuffered := false,
    Test / testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v"),
    Test / testOptions += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "5"),

    // Code coverage
    scoverage.ScoverageKeys.coverageExcludedPackages := "<empty>;.*Test.*;.*testing.*",

    fork := true
  )

lazy val examples = (project in file("examples"))
  .dependsOn(foresight)
  .settings(
    name := "foresight-examples",
    commonScalaSettings,

    // Dependencies
    libraryDependencies ++= Dependencies.libraryDependencies(scalaVersion.value),

    // Testing
    libraryDependencies ++= Dependencies.testDependencies(scalaVersion.value).map(_ % Test),
    Test / logBuffered := false,
    Test / testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v"),
    Test / testOptions += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "5"),

    // Code coverage
    scoverage.ScoverageKeys.coverageExcludedPackages := "<empty>;.*Test.*;.*testing.*",

    fork := true
  )


lazy val benchmarks = (project in file("benchmarks"))
  .enablePlugins(JmhPlugin)
  .dependsOn(foresight, examples)
  .settings(
    name := "foresight-benchmarks",
    commonScalaSettings,
    publish / skip := true,
//    Test / skip := true,
    Jmh / fork := true,
    Jmh / javaOptions ++= Seq("-Xms4G", "-Xmx4G")
  )

// Aggregated Scaladoc for the root project (copies child docs under one folder)
lazy val aggregatedDoc = taskKey[File]("Generate aggregated Scaladoc for the root from subprojects")

lazy val root = (project in file("."))
  .aggregate(foresight, examples, benchmarks)
  .settings(
    name := "foresight-root",
    commonScalaSettings,
    // Make `root / doc` build and collect child docs without publishing any package
    Compile / doc := aggregatedDoc.value,
    aggregatedDoc := {
      val log = streams.value.log
      val sv   = scalaVersion.value
      val sbin = scalaBinaryVersion.value
      val scalaDir = if (sbin.startsWith("3")) s"scala-$sv" else s"scala-$sbin"
      val out = target.value / scalaDir / "api"
      sbt.IO.delete(out)

      // Trigger docs for each child explicitly (avoid ScopeFilter + lambda captures)
      val foresightDocOut  = (foresight  / Compile / doc).value
      val examplesDocOut   = (examples   / Compile / doc).value
      val benchmarksDocOut = (benchmarks / Compile / doc).value

      // Copy into per-project subfolders under the aggregate
      sbt.IO.copyDirectory(foresightDocOut,  out / foresight.id)
      sbt.IO.copyDirectory(examplesDocOut,   out / examples.id)
      sbt.IO.copyDirectory(benchmarksDocOut, out / benchmarks.id)

      log.info(s"Aggregated docs written to: $out")
      out
    },
    publish / skip := true,            // don't publish the aggregate
    Compile / packageDoc / publishArtifact := false,
    Compile / packageSrc / publishArtifact := false
  )

// Publish to GitHub Pages
enablePlugins(GhpagesPlugin, SitePlugin, SiteScaladocPlugin)
git.remoteRepo := "https://github.com/jonathanvdc/foresight.git"
