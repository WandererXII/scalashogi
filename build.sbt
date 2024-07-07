name := "scalashogi"

version := "12.1.1"

ThisBuild / scalaVersion := "2.13.14"
ThisBuild / githubWorkflowPublishTargetBranches := Seq() // Don't publish anywhere
ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("17"))
ThisBuild / githubWorkflowBuild ++= Seq(
  WorkflowStep.Sbt(List("scalafmtCheckAll"), name = Some("Check Formatting"))
)

libraryDependencies ++= List(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
  "org.specs2"             %% "specs2-core"              % "4.20.8" % Test,
  "org.specs2"             %% "specs2-cats"              % "4.20.8" % Test,
  "com.github.ornicar"     %% "scalalib"                 % "7.0.2",
  "joda-time"              % "joda-time"                 % "2.12.7",
  "org.typelevel"          %% "cats-core"                % "2.12.0"
)

resolvers ++= Seq(
  "lila-maven" at "https://raw.githubusercontent.com/lichess-org/lila-maven/master"
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
  // Warnings as errors!
  "-Xfatal-warnings",
  // Linting options
  "-unchecked",
  "-Xcheckinit",
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
  // "-Wnumeric-widen",
  "-Wunused:imports",
  "-Wunused:locals",
  "-Wunused:patvars",
  "-Wunused:privates",
  "-Wunused:implicits",
  "-Wunused:params",
  "-Wvalue-discard",
  "-Xmaxerrs",
  "12"
)
