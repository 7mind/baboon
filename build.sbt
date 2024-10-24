import sbtrelease.ReleasePlugin.autoImport.ReleaseTransformations.*

ThisBuild / scalaVersion := "2.13.14"

val niOptionsCommon = Seq(
  "-ENIX_BINTOOLS",
  "-ENIX_CC",
  "-ENIX_CFLAGS_COMPILE",
  "-ENIX_LDFLAGS",
  "-EbuildInputs",
  "-EcmakeFlags",
  "-EnativeBuildInputs",
  "-EpropagatedBuildInputs",
  "-EpropagatedNativeBuildInputs",
)

val niOptions = niOptionsCommon ++ (if (System
                                          .getProperty("os.name")
                                          .toLowerCase
                                          .contains("mac") && System
                                          .getProperty("os.arch") == "aarch64") {
                                      Seq(
                                        "-ENIX_CC_WRAPPER_TARGET_HOST_aarch64_apple_darwin",
                                        "-ENIX_BINTOOLS_WRAPPER_TARGET_HOST_aarch64_apple_darwin",
                                      )
                                    } else if (System
                                                 .getProperty("os.name")
                                                 .toLowerCase
                                                 .contains("mac")) {
                                      Seq(
                                        "-ENIX_CC_WRAPPER_TARGET_HOST_x86_64_apple_darwin",
                                        "-ENIX_BINTOOLS_WRAPPER_TARGET_HOST_x86_64_apple_darwin",
                                      )
                                    } else {
                                      Seq.empty
                                    })

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
    graalVMNativeImageOptions ++= niOptions,
    graalVMNativeImageOptions ++= Seq(
      "-H:+UnlockExperimentalVMOptions",
      "--no-fallback",
      "-H:+ReportExceptionStackTraces",
      "--report-unsupported-elements-at-runtime",
      "--enable-https",
      "--enable-http",
      "-march=compatibility"
    ),
    run / fork := true,
    scalacOptions ++= Seq(
      s"-Xmacro-settings:product-name=${name.value}",
      s"-Xmacro-settings:product-version=${version.value}",
      s"-Xmacro-settings:product-group=${organization.value}",
      s"-Xmacro-settings:scala-version=${scalaVersion.value}",
      s"-Xmacro-settings:scala-versions=${crossScalaVersions.value.mkString(":")}"
    ),
    releaseProcess := Seq[ReleaseStep](
      checkSnapshotDependencies, // : ReleaseStep
      inquireVersions, // : ReleaseStep
      runClean, // : ReleaseStep
      runTest, // : ReleaseStep
      setReleaseVersion, // : ReleaseStep
      commitReleaseVersion, // : ReleaseStep, performs the initial git checks
      tagRelease, // : ReleaseStep
      //publishArtifacts,                       // : ReleaseStep, checks whether `publishTo` is properly set up
      setNextVersion, // : ReleaseStep
      commitNextVersion, // : ReleaseStep
      pushChanges // : ReleaseStep, also checks that an upstream branch is properly configured
    )
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
