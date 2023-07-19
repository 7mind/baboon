ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.11"

lazy val root = (project in file("."))
  .settings(
    name := "baboon",
    libraryDependencies ++= Seq("com.lihaoyi" %% "fastparse" % "3.0.1"),
    libraryDependencies ++= Seq(
      "fundamentals-platform",
      "fundamentals-functional",
      "fundamentals-language",
      "fundamentals-collections",
    ).map("io.7mind.izumi" %% _ % "1.1.0-M25"),
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.3.0"
    ),
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.16" % Test,
    ),
//    idePackagePrefix := Some("io.septimalmind.baboon"),
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
      "-Xsource:3",
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
      "-Wconf:any:warning",
      "-Wconf:cat=optimizer:warning",
      "-Wconf:cat=other-match-analysis:error",
      "-Vtype-diffs",
    ),
  )
