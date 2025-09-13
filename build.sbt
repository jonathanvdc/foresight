ThisBuild / crossScalaVersions := Seq("2.11.12", "2.12.20", "2.13.14", "3.4.1")
ThisBuild / scalaVersion := "3.4.1"

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

lazy val foresight = (project in file("."))
  .settings(
    name := "foresight",

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

    scalacOptions in(Compile, doc) := Seq("-implicits"),

    // Source locations
//    scalaSource in Compile := baseDirectory(_ / "src/main").value,
//    scalaSource in Test := baseDirectory(_ / "src/test").value,
//    javaSource in Compile := baseDirectory(_ / "src/main").value,
//    javaSource in Test := baseDirectory(_ / "src/test").value,

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

lazy val bench = (project in file("bench"))
  .enablePlugins(JmhPlugin)
  .dependsOn(foresight)
  .settings(
    name := "foresight-bench",
    publish / skip := true,
    Test / skip := true,
    Jmh / fork := true,
    Jmh / javaOptions ++= Seq("-Xms4G", "-Xmx4G"),

    Compile / unmanagedSourceDirectories ++= {
      val base = (Compile / sourceDirectory).value
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 11)) | Some((2, 12)) => Seq(base / "scala-2.11")
        case Some((2, 13))                 => Seq(base / "scala-2.13+")
        case Some((3, _))                  => Seq(base / "scala-2.13+", base / "scala-3")
        case _                             => Nil
      }
    },
  )

// Publish to GitHub Pages
enablePlugins(GhpagesPlugin, SitePlugin, SiteScaladocPlugin)
git.remoteRepo := "https://github.com/jonathanvdc/foresight.git"
