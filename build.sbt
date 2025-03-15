ThisBuild / scalaVersion := "2.11.12"

lazy val arithExpr = (project in file("."))
  .settings(
    name          := "fixpoint",
    version       := "1.0",

    scalacOptions ++= Seq("-Xmax-classfile-name", "100", "-unchecked", "-deprecation", "-feature"),
    scalacOptions in (Compile, doc) := Seq("-implicits", "-diagrams"),

    // Source locations (defaults would be: src/main/scala and test/main/java)
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
