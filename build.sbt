import sbtrelease.ReleasePlugin.autoImport.ReleaseTransformations.*

import scala.sys.process._
lazy val refreshFlakeTask = taskKey[Unit]("Refresh flake.nix")
lazy val isMacOS: Boolean = System.getProperty("os.name").toLowerCase.contains("mac")

ThisBuild / scalaVersion := "2.13.15"

lazy val root = (project in file("."))
  .settings(
    name := "baboon",
    libraryDependencies ++= Seq("com.lihaoyi" %% "fastparse" % "3.1.0"),
    libraryDependencies ++= Seq(
      "fundamentals-platform",
      "fundamentals-functional",
      "fundamentals-language",
      "fundamentals-collections",
      "distage-core",
      "distage-testkit-scalatest",
    ).map("io.7mind.izumi" %% _ % "1.2.8"),
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.3.0"
    ),
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.18" % Test,
    ),
    libraryDependencies ++= Seq(
      "com.github.alexarchambault" %% "case-app" % "2.1.0-M29"
    ),
//    libraryDependencies ++= Seq(
//      "org.graalvm.buildtools" % "graalvm-reachability-metadata" % "0.10.2"
//    ),
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-generic",
      "io.circe" %% "circe-parser"
    ).map(_ % "0.14.1"),
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
      "-H:-CheckToolchain", // fixes Darwin builds under Nix
      "-H:+UnlockExperimentalVMOptions",
      "--no-fallback",
      "-H:+ReportExceptionStackTraces",
      "--report-unsupported-elements-at-runtime",
      "--enable-https",
      "--enable-http",
      "-march=compatibility"
    ),
    graalVMNativeImageOptions ++= {
      if (isMacOS) Seq("-H:CCompilerOption=-mmacosx-version-min=11.0")
      else Seq.empty
    },
    run / fork := true,
    scalacOptions ++= Seq(
      s"-Xmacro-settings:product-name=${name.value}",
      s"-Xmacro-settings:product-version=${version.value}",
      s"-Xmacro-settings:product-group=${organization.value}",
      s"-Xmacro-settings:scala-version=${scalaVersion.value}",
      s"-Xmacro-settings:scala-versions=${crossScalaVersions.value.mkString(":")}"
    ),
    refreshFlakeTask := {
              val log = streams.value.log
              val result = "./build.sh nix flake-refresh flake-validate" ! log
              if (result != 0) {
                throw new MessageOnlyException("flake.nix update failed!")
              }
            },
    releaseProcess := Seq[ReleaseStep](
      checkSnapshotDependencies,
      inquireVersions,
      runClean,
      runTest,
      setReleaseVersion,
      releaseStepTask(refreshFlakeTask),
      commitReleaseVersion,
      tagRelease,
      //publishArtifacts,
      setNextVersion,
      commitNextVersion,
      pushChanges
    ),
  )

ThisBuild / scalacOptions ++= Seq(
  s"-Xmacro-settings:sbt-version=${sbtVersion.value}",
  s"-Xmacro-settings:git-repo-clean=${com.github.sbt.git.SbtGit.GitKeys.gitUncommittedChanges.value}",
  s"-Xmacro-settings:git-branch=${com.github.sbt.git.SbtGit.GitKeys.gitCurrentBranch.value}",
  s"-Xmacro-settings:git-described-version=${com.github.sbt.git.SbtGit.GitKeys.gitDescribedVersion.value
    .getOrElse("")}",
  s"-Xmacro-settings:git-head-commit=${com.github.sbt.git.SbtGit.GitKeys.gitHeadCommit.value.getOrElse("")}"
)

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/7mind/baboon"),
    "scm:git@github.com:7mind/baboon.git"
  )
)

ThisBuild / organization := "io.7mind"
