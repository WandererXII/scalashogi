ThisBuild / organization := "io.github.WandererXII"
ThisBuild / organizationName := "WandererXII"
ThisBuild / organizationHomepage := Some(url("https://lishogi.org"))

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/WandererXII/scalashogi"),
    "scm:git@github.WandererXII/scalashogi.git"
  )
)

ThisBuild / developers := List(
  Developer(
    id    = "WandererXII",
    name  = "Daniel Liska",
    email = "daniel.w.liska@gmail.com",
    url   = url("https://lishogi.org")
  )
)

ThisBuild / description := "Shogi API written in scala. Immutable and free of side effects."
ThisBuild / licenses := List("The MIT license" -> new URL("https://github.com/WandererXII/scalashogi/blob/main/LICENSE"))
ThisBuild / homepage := Some(url("https://github.com/WandererXII/scalashogi"))

// Remove all additional repository other than Maven Central from POM
ThisBuild / pomIncludeRepository := { _ => false }

ThisBuild / publishTo := {
  val nexus = "https://s01.oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

ThisBuild / publishMavenStyle := true

ThisBuild / versionScheme := Some("early-semver")