organization         := "io.github.wandererxii"
organizationName     := "WandererXII"
organizationHomepage := Some(uri("https://lishogi.org"))

scmInfo := Some(
  ScmInfo(
    uri("https://github.com/WandererXII/scalashogi"),
    "scm:git@github.com:WandererXII/scalashogi.git",
  ),
)

developers := List(
  Developer(
    id = "WandererXII",
    name = "Daniel Liska",
    email = "daniel.w.liska@gmail.com",
    url = uri("https://lishogi.org"),
  ),
)

description := "Shogi API written in scala. Immutable and free of side effects."
licenses    := List(
  sbt.librarymanagement.License(
    "MIT",
    uri("https://github.com/WandererXII/scalashogi/blob/main/LICENSE"),
  ),
)
homepage := Some(uri("https://github.com/WandererXII/scalashogi"))

// Remove all additional repository other than Maven Central from POM
pomIncludeRepository := { _ => false }

publishTo := {
  val centralSnapshots = "https://central.sonatype.com/repository/maven-snapshots/"
  if (isSnapshot.value) Some("central-snapshots" at centralSnapshots)
  else localStaging.value
}

publishMavenStyle := true

versionScheme := Some("strict")
