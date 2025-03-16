import sbt._

object Dependencies {
    lazy val libraryDependencies = Seq(
        // scala
        "org.scala-lang" % "scala-reflect" % "2.11.12",
        "org.scala-lang" % "scala-compiler" % "2.11.12",
        "org.scala-lang" % "scala-library" % "2.11.12",
    // testing
        "junit" % "junit" % "4.11",
        "com.novocode" % "junit-interface" % "0.11" % "test",
        "org.scalacheck" %% "scalacheck" % "1.13.0" % "test",
    // XML
        "org.scala-lang.modules" % "scala-xml_2.11" % "1.0.4",
    )
}