name := "scalashogi"

version := "12.2.0"

ThisBuild / scalaVersion := "2.13.16"

libraryDependencies ++= List(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
  "org.typelevel"          %% "cats-core"                % "2.13.0",
  "joda-time"               % "joda-time"                % "2.14.0",
  "org.specs2"             %% "specs2-core"              % "4.21.0" % Test,
  "org.specs2"             %% "specs2-cats"              % "4.21.0" % Test,
)

scalacOptions ++= Seq(
  "-encoding",
  "utf-8",
  "-explaintypes",
  "-feature",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-Ymacro-annotations",
  "-unchecked",
  "-Xcheckinit",
  // Linting options
  "-Xlint:adapted-args",
  "-Xlint:constant",
  "-Xlint:delayedinit-select",
  "-Xlint:deprecation",
  "-Xlint:inaccessible",
  "-Xlint:infer-any",
  "-Xlint:missing-interpolator",
  "-Xlint:nullary-unit",
  "-Xlint:option-implicit",
  "-Xlint:package-object-classes",
  "-Xlint:poly-implicit-overload",
  "-Xlint:private-shadow",
  "-Xlint:stars-align",
  "-Xlint:type-parameter-shadow",
  "-Wdead-code",
  "-Wextra-implicit",
  "-Wnumeric-widen",
  "-Wunused:imports",
  "-Wunused:locals",
  "-Wunused:patvars",
  "-Wunused:privates",
  "-Wunused:implicits",
  "-Wunused:params",
  "-Wvalue-discard",
  "-Werror",
)
