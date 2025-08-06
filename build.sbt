ThisBuild / crossScalaVersions := Seq("2.11.12", "2.13.14", "3.4.1")
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
        case Some((2, 11)) =>
          Seq("-unchecked", "-deprecation", "-feature", "-Xmax-classfile-name", "100")
        case Some((2, _)) =>
          Seq("-unchecked", "-deprecation", "-feature")
        case Some((3, 4)) =>
          Seq(
            "-unchecked",
            "-deprecation",
            "-feature",
            "-Wconf:msg=with.*is deprecated:silent",
            "-Wconf:msg=The syntax `x: _\\*`.*no longer supported:silent"
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
        case Some((2, 11)) => Seq(base / "scala-2.11")
        case Some((2, 13)) => Seq(base / "scala-2.13+")
        case Some((3, _))  => Seq(base / "scala-2.13+")
        case _             => Nil
      }
    },

    // Dependencies
    libraryDependencies ++= Dependencies.libraryDependencies(scalaVersion.value),

    // Testing
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v"),
    testOptions += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "5"),

    // Code coverage
    scoverage.ScoverageKeys.coverageExcludedPackages := "<empty>;.*Test.*;.*testing.*",

    fork := true
  )

// Publish to GitHub Pages
enablePlugins(GhpagesPlugin, SitePlugin, SiteScaladocPlugin)
git.remoteRepo := "https://github.com/jonathanvdc/foresight.git"
