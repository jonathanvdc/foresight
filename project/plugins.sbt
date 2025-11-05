resolvers += Classpaths.sbtPluginReleases

val addPluginIf =
  (cond: Boolean) => (m: ModuleID) => if (cond) Seq(addSbtPlugin(m)) else Seq()

val enableGitPlugins = !sys.env.get("NO_GIT").contains("1")
val addIfGit = addPluginIf(enableGitPlugins)

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.1")
addIfGit("com.typesafe.sbt" % "sbt-site" % "1.4.1")
addIfGit("com.typesafe.sbt" % "sbt-ghpages" % "0.6.3")
// addSbtPlugin("com.typesafe.sbt" % "sbt-pgp" % "2.1.2")
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.7")
