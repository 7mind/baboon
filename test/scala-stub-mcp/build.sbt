// Scala MCP round-trip test project (T12).
//
// Minimal project for testing the generated Scala MCP server dispatch.
// Generated files are placed under src/main/scala/generated-main/.
// This project does NOT have -Wconf:any:error so pre-existing generated-code
// quirks (e.g. unused imports for foreign-type shims) compile as warnings only.
ThisBuild / scalaVersion     := "2.13.16"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "scala-stub-mcp",
    Test / parallelExecution := false,
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-generic",
      "io.circe" %% "circe-parser",
    ).map(_ % "0.14.13"),
    addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.3" cross CrossVersion.full),
    scalacOptions ++= Seq(
      "-Wconf:cat=other-match-analysis:error",
      "-encoding",
      "utf8",
      "-deprecation",
      "-feature",
      "-unchecked",
      "-language:experimental.macros",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-explaintypes",
      "-Xsource:3-cross",
      "-Wdead-code",
      "-Wextra-implicit",
      "-Wnumeric-widen",
      "-Woctal-literal",
      "-Wvalue-discard",
      "-Wunused:_",
      "-Wmacros:after",
      "-Ycache-plugin-class-loader:always",
      "-Ycache-macro-class-loader:last-modified",
      "-Wconf:msg=nowarn:silent",
      "-Wconf:any:error",
      // Demote unused-import to warning (not error) AFTER the general any:error:
      // in Scala 2.13, -Wconf uses LAST-match-wins, so this overrides any:error
      // specifically for unused imports. Generated foreign-type imports may be
      // structurally "unused" when the type maps to a primitive (site=<empty>).
      "-Wconf:cat=unused-imports:warning",
      "-Wconf:cat=deprecation:warning",
      "-Wconf:cat=optimizer:warning",
      "-Wconf:cat=other-match-analysis:error",
      "-Vtype-diffs",
      "-P:kind-projector:underscore-placeholders"
    ),
  )
