ThisBuild / scalaVersion := "2.11.12"
ThisBuild / organization := "com.github.jonathanvdc"
ThisBuild / version := {
    val fallback = "0.1.0-SNAPSHOT"
    val gitVersion = try {
        val v = sys.process.Process("git describe --tags --always --dirty").lineStream_!.head
        v.stripPrefix("v")
    } catch {
        case _: Exception => fallback
    }
    gitVersion
}

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
      name          := "foresight",

      scalacOptions ++= Seq("-Xmax-classfile-name", "100", "-unchecked", "-deprecation", "-feature"),
      scalacOptions in (Compile, doc) := Seq("-implicits", "-diagrams"),

      // Source locations
      scalaSource in Compile := baseDirectory(_ / "src/main").value,
      scalaSource in Test := baseDirectory(_ / "src/test").value,
      javaSource in Compile := baseDirectory(_ / "src/main").value,
      javaSource in Test := baseDirectory(_ / "src/test").value,

      // dependencies specified in project/Dependencies.scala
      libraryDependencies ++= Dependencies.libraryDependencies,

      testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v"),
      testOptions += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "5"),

      scoverage.ScoverageKeys.coverageExcludedPackages := "<empty>;.*Test.*;.*testing.*",

      fork := true
  )
