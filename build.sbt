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
//    libraryDependencies ++= Seq("com.lihaoyi" % "mainargs_2.13" % "0.5.0"),
    libraryDependencies ++= Seq(
      "com.github.alexarchambault" %% "case-app" % "2.1.0-M25"
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
  .enablePlugins(GraalVMNativeImagePlugin, UniversalPlugin)
  .settings(
    GraalVMNativeImage / mainClass := Some("io.septimalmind.baboon.Baboon"),
    graalVMNativeImageOptions ++= Seq(
      "--no-fallback",
      "-H:+ReportExceptionStackTraces",
      "--report-unsupported-elements-at-runtime",
      "--enable-https",
      "--enable-http",
    ),
    run / fork := true,
    scalacOptions ++= Seq(
      s"-Xmacro-settings:product-name=${name.value}",
      s"-Xmacro-settings:product-version=${version.value}",
      s"-Xmacro-settings:product-group=${organization.value}",
      s"-Xmacro-settings:scala-version=${scalaVersion.value}",
      s"-Xmacro-settings:scala-versions=${crossScalaVersions.value.mkString(":")}"
    ),
  )

ThisBuild / scalacOptions ++= Seq(
  s"-Xmacro-settings:sbt-version=${sbtVersion.value}",
  s"-Xmacro-settings:git-repo-clean=${com.github.sbt.git.SbtGit.GitKeys.gitUncommittedChanges.value}",
  s"-Xmacro-settings:git-branch=${com.github.sbt.git.SbtGit.GitKeys.gitCurrentBranch.value}",
  s"-Xmacro-settings:git-described-version=${com.github.sbt.git.SbtGit.GitKeys.gitDescribedVersion.value.getOrElse("")}",
  s"-Xmacro-settings:git-head-commit=${com.github.sbt.git.SbtGit.GitKeys.gitHeadCommit.value.getOrElse("")}"
)