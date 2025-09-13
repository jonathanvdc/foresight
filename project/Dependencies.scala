import sbt._

object Dependencies {
  private def isScala211(scalaVersion: String): Boolean =
    scalaVersion.startsWith("2.11")

  private def isScala213Plus(scalaVersion: String): Boolean =
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, v)) if v >= 13 => true
      case Some((3, _)) => true
      case _ => false
    }

  def libraryDependencies(scalaVersion: String): Seq[ModuleID] = {
    val scalaXmlVersion = if (isScala211(scalaVersion)) "1.0.4" else "2.1.0"

    val baseDeps = Seq(
      "org.scala-lang.modules" %% "scala-xml" % scalaXmlVersion
    )

    val reflectDeps =
      if (isScala211(scalaVersion))
        Seq(
          "org.scala-lang" % "scala-reflect" % scalaVersion,
          "org.scala-lang" % "scala-compiler" % scalaVersion
        )
      else Nil

    val parallelCollections =
      if (isScala213Plus(scalaVersion))
        Seq("org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4")
      else Nil

    baseDeps ++ reflectDeps ++ parallelCollections
  }

  def testDependencies(scalaVersion: String): Seq[ModuleID] = {
    val scalaCheckVersion = if (isScala211(scalaVersion)) "1.14.3" else "1.17.0"

    Seq(
      "junit" % "junit" % "4.11",
      "com.novocode" % "junit-interface" % "0.11",
      "org.scalacheck" %% "scalacheck" % scalaCheckVersion
    )
  }
}
