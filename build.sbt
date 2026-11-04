name := "scalashogi"

version := "13.3.0-clock-migration-SNAPSHOT"

scalaVersion := "2.13.18"

libraryDependencies ++= List(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
  "org.typelevel"          %% "cats-core"                % "2.13.0",
  "org.specs2"             %% "specs2-core"              % "4.23.0" % Test,
  "org.specs2"             %% "specs2-cats"              % "4.23.0" % Test,
)

// Keep specs2's transitive dependency from upgrading the parser library.
dependencyOverrides += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

scalacOptions ++= Seq(
  "-encoding",
  "utf-8",
  "-release",
  "21",
  "-explaintypes",
  "-feature",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-unchecked",
  "-Xlint:adapted-args",
  "-Xlint:byname-implicit",
  "-Xlint:constant",
  "-Xlint:delayedinit-select",
  "-Xlint:deprecation",
  "-Xlint:eta-zero",
  "-Xlint:implicit-recursion",
  "-Xlint:inaccessible",
  "-Xlint:infer-any",
  "-Xlint:int-div-to-float",
  "-Xlint:missing-interpolator",
  "-Xlint:nullary-unit",
  "-Xlint:option-implicit",
  "-Xlint:package-object-classes",
  "-Xlint:pattern-shadow",
  "-Xlint:poly-implicit-overload",
  "-Xlint:private-shadow",
  "-Xlint:recurse-with-default",
  "-Xlint:stars-align",
  "-Xlint:type-parameter-shadow",
  "-Xlint:valpattern",
  "-Wdead-code",
  "-Wextra-implicit",
  "-Wnumeric-widen",
  "-Wunused:imports",
  "-Wunused:locals",
  "-Wunused:patvars",
  "-Wunused:privates",
  "-Wunused:params",
  "-Wvalue-discard",
  "-Werror",
)

// Runtime initialization checks are a debugging aid; keep them out of published bytecode.
Test / scalacOptions += "-Xcheckinit"
