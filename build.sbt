import sbtrelease.ReleasePlugin.autoImport.ReleaseTransformations.*
import org.scalajs.linker.interface.ModuleSplitStyle
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

import scala.sys.process.*

lazy val refreshFlakeTask = taskKey[Unit]("Refresh flake.nix")
// lazy val isMacOS: Boolean = System.getProperty("os.name").toLowerCase.contains("mac")

ThisBuild / scalaVersion := "2.13.16"

// Shared settings for both JVM and JS
lazy val sharedSettings = Seq(
  name := "baboon",
  libraryDependencies ++= Seq("com.lihaoyi" %%% "fastparse" % "3.1.1"),
  libraryDependencies ++= Seq(
    "fundamentals-platform",
    "fundamentals-functional",
    "fundamentals-language",
    "fundamentals-collections",
    "distage-core",
  ).map("io.7mind.izumi" %%% _ % "1.2.21"),
  libraryDependencies ++= Seq(
    "org.scala-lang.modules" %%% "scala-parser-combinators" % "2.4.0"
  ),
  libraryDependencies ++= Seq(
    "org.scalatest" %%% "scalatest" % "3.2.19" % Test,
  ),
  libraryDependencies ++= Seq(
    "io.circe" %%% "circe-core",
    "io.circe" %%% "circe-generic",
    "io.circe" %%% "circe-parser"
  ).map(_ % "0.14.1"),
  libraryDependencies ++= Seq(
    "com.softwaremill.magnolia1_2" %%% "magnolia" % "1.1.10",
    "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided"
  ),
  addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.3" cross CrossVersion.full),
  scalacOptions ++= Seq(
    s"-Xmacro-settings:product-name=${name.value}",
    s"-Xmacro-settings:product-version=${version.value}",
    s"-Xmacro-settings:product-group=${organization.value}",
    s"-Xmacro-settings:scala-version=${scalaVersion.value}",
    s"-Xmacro-settings:scala-versions=${crossScalaVersions.value.mkString(":")}"
  ),
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
    "-Wconf:any:warning",
    "-Wconf:cat=optimizer:warning",
    "-Wconf:cat=other-match-analysis:error",
    "-Vtype-diffs",
    "-P:kind-projector:underscore-placeholders"
  )
)

// Cross-platform project with CrossType.Pure
lazy val baboon = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("baboon-compiler"))
  .settings(sharedSettings)
  .jvmSettings(
    Compile / unmanagedResourceDirectories += baseDirectory.value / "src" / "main" / "resources",
    libraryDependencies ++= Seq(
      "com.github.alexarchambault" %% "case-app" % "2.1.0-M30",
      "io.7mind.izumi" %% "distage-testkit-scalatest" % "1.2.21" % Test
    )
  )
  .jvmConfigure(_.enablePlugins(GraalVMNativeImagePlugin, UniversalPlugin))
  .jvmSettings(
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
    run / fork := true,
    refreshFlakeTask := {
      val log = streams.value.log
      val result = "nix develop --command mdl :flake-refresh" ! log
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
    )
  )
  .jsSettings(
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
       .withModuleSplitStyle(ModuleSplitStyle.SmallestModules)
    },
    scalaJSUseMainModuleInitializer := false,
    libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.6.0",
  )

// Define JVM and JS projects
lazy val baboonJVM = baboon.jvm
lazy val baboonJS = baboon.js

// Root aggregate project
lazy val root = project
  .in(file("."))
//  .aggregate(baboonJVM, baboonJS)
  .aggregate(baboonJVM)
  .settings(
    name := "baboon-root",
    publish / skip := true,
    publishLocal / skip := true,
    Compile / sources := Seq.empty,
    Test / sources := Seq.empty
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

ThisBuild / organization := "io.7mind.izumi"

ThisBuild / publishTo := {
  if (isSnapshot.value) {
    Some(
      "central-snapshots" at "https://central.sonatype.com/repository/maven-snapshots/"
    )
  } else {
    localStaging.value
  }
}

ThisBuild / credentials ++= Seq(
  Path.userHome / ".sbt" / "secrets" / "credentials.sonatype-new.properties",
  Path.userHome / ".sbt" / "secrets" / "credentials.sonatype-nexus.properties",
  file(".") / ".secrets" / "credentials.sonatype-nexus.properties",
)
  .filter(_.exists())
  .map(Credentials.apply)

ThisBuild / homepage := Some(url("https://github.com/7mind/baboon"))
ThisBuild / licenses := Seq(
  "BSD-style" -> url("http://www.opensource.org/licenses/mit-license.php")
)
ThisBuild / developers := List(
  Developer(
    id = "7mind",
    name = "Septimal Mind",
    url = url("https://github.com/7mind"),
    email = "team@7mind.io",
  )
)
ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/7mind/baboon"),
    "scm:git:https://github.com/7mind/baboon.git",
  )
)
