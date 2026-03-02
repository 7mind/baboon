lazy val root = (project in file("."))
  .settings(
    name := "petstore-service-scala",
    scalaVersion := "2.13.16",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core"    % "0.14.13",
      "io.circe" %% "circe-generic" % "0.14.13",
      "io.circe" %% "circe-parser"  % "0.14.13",
    ),
    addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.13.3" cross CrossVersion.full),
    scalacOptions ++= Seq(
      "-encoding", "utf8",
      "-feature",
      "-unchecked",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-Xsource:3-cross",
      "-P:kind-projector:underscore-placeholders",
    ),
    Compile / mainClass := Some("example.Main"),
    run / fork := true,
  )
