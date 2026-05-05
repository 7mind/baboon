ThisBuild / scalaVersion     := "2.13.16"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "sc-stub",
    // KeyCodecHostLastWinsSpec mutates a process-global mutable singleton
    // (`FStr_KeyCodec.instance`) that other suites (e.g. ForeignMapKeyRoundTripSpec)
    // read concurrently when scalatest runs suites in parallel — observed Windows-only
    // CI flake at PR-32.2-D01 (2026-05-05). The `after` teardown restores identity but
    // cannot prevent in-flight reads during the test body. Serialise the stub.
    Test / parallelExecution := false,
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test",

      libraryDependencies ++=  Seq(
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
      "-Wconf:cat=deprecation:warning",
      "-Wconf:cat=optimizer:warning",
      "-Wconf:cat=other-match-analysis:error",
      "-Vtype-diffs",
      "-P:kind-projector:underscore-placeholders"
    ),
  )

