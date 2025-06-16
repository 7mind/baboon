package io.septimalmind.baboon.scstub

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.wordspec.AnyWordSpec
import java.nio.file.{Files, Paths}

class GeneratedCodecTestsSpec extends AnyWordSpec {

  // Define the expected output directory for generated test files.
  // This path will need to be adjusted based on the actual generator's output structure.
  // Assuming the generator outputs to a path like:
  // target/scala-2.13/src_managed/test/scstub/codectests/
  // The exact path depends on sbt-buildinfo or other plugins if used by the main Baboon build.
  // For now, let's make it relative to a hypothetical "generated-sources" root.
  val generatedTestSourcesBaseDir = Paths.get("target/baboon/generated-test-sources/scstub/codectests") // Placeholder

  // List of expected generated test files based on sc_codec_test_defs.baboon
  val expectedTestFiles = Seq(
    "MyTestEnum_Tests.scala",
    "MyTestDto_Tests.scala",
    "MyTestAdt_Tests.scala",
    "MyComplexDto_Tests.scala"
    // Note: ADT members (BranchA, BranchB) might also have their own _Tests.scala files
    // if they are treated as independent definitions by the test generator.
    // For now, assuming only root definitions get test files.
    // If MyTestAdt.BranchA and MyTestAdt.BranchB are generated as top-level like classes,
    // they might also have tests:
    // "BranchA_Tests.scala",
    // "BranchB_Tests.scala"
    // This depends on how ScDefnTranslator and ScCodecTestsTranslator handle ADT members.
    // Based on ScDefnTranslator logic, ADT members are usually part of the ADT's file or not generated standalone.
    // The CSDefnTranslator skips ADT members if `useCompactAdtForm` is true.
    // ScDefnTranslator skips Owner.Adt(_) for translateTests.
    // So, likely no separate files for BranchA, BranchB.
  )

  "Generated Codec Tests" should {
    "exist in the expected output directory" in {
      // This is a placeholder for where the Baboon compiler would be invoked.
      // In a real build, this would happen before tests run.
      // For this subtask, we are just checking for pre-generated files.

      var allFilesFound = true
      val missingFiles = scala.collection.mutable.ListBuffer[String]()

      expectedTestFiles.foreach { fileName =>
        val filePath = generatedTestSourcesBaseDir.resolve(fileName)
        if (!Files.exists(filePath)) {
          allFilesFound = false
          missingFiles += filePath.toString
          println(s"Missing expected generated test file: $filePath")
        } else {
          println(s"Found expected generated test file: $filePath")
        }
      }

      if (!allFilesFound) {
        fail(s"Missing generated test files: ${missingFiles.mkString(", ")}")
      }
    }

    // Advanced: Placeholder for a test that tries to compile generated files
    // "compile successfully" in {
    //   // This would require:
    //   // 1. Knowing the exact output paths of generated files.
    //   // 2. Invoking the Scala compiler (e.g., via sbt, or programmatically).
    //   // 3. Checking for compilation errors.
    //   // This is likely too complex for the current subtask.
    //   succeed // Placeholder
    // }
  }
}
package io.septimalmind.baboon.scstub

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.wordspec.AnyWordSpec
import java.nio.file.{Files, Paths}

class GeneratedCodecTestsSpec extends AnyWordSpec {

  // Define the expected output directory for generated test files.
  // This path will need to be adjusted based on the actual generator's output structure.
  // Assuming the generator outputs to a path like:
  // target/scala-2.13/src_managed/test/scstub/codectests/
  // The exact path depends on sbt-buildinfo or other plugins if used by the main Baboon build.
  // For now, let's make it relative to a hypothetical "generated-sources" root.
  // The actual path for ScDefnTranslator output is like:
  // s"$fbase/${defn.id.name.name.capitalize}${suffix.getOrElse("")}.scala"
  // where fbase is like "baboon-out/version/domain/subdomain"
  // and suffix is ".tests"
  // So, for model scstub.codectests, version 1.0.0, it would be something like:
  // baboon-out/1.0.0/scstub/codectests/MyTestEnum.tests.scala
  // Let's adjust the base directory accordingly.
  val generatedTestSourcesBaseDir = Paths.get("baboon-out/1.0.0/scstub/codectests")


  // List of expected generated test files based on sc_codec_test_defs.baboon
  // Filenames are based on ScDefnTranslator.getOutputPath
  val expectedTestFiles = Seq(
    "MyTestEnum.tests.scala",
    "MyTestDto.tests.scala",
    "MyTestAdt.tests.scala",
    "MyComplexDto.tests.scala"
    // ADT members (BranchA, BranchB) are part of the ADT definition file in Scala,
    // and ScDefnTranslator.translateTests filters out Owner.Adt members,
    // so they won't have their own separate test files.
  )

  "Generated Codec Test files" should {
    "exist in the expected output directory after Baboon compilation" in {
      // Note: This test assumes that the Baboon compilation process has already run
      // and generated the test files. In a typical sbt setup, code generation
      // would be a task that runs before the `test` task.

      var allFilesFound = true
      val missingFiles = scala.collection.mutable.ListBuffer[String]()

      expectedTestFiles.foreach { fileName =>
        val filePath = generatedTestSourcesBaseDir.resolve(fileName)
        if (!Files.exists(filePath)) {
          allFilesFound = false
          missingFiles += filePath.toString
          System.err.println(s"Missing expected generated test file: $filePath")
        } else {
          System.out.println(s"Found expected generated test file: $filePath")
        }
      }

      if (!allFilesFound) {
        fail(s"Missing generated test files: ${missingFiles.mkString(", ")}. " +
             s"Please ensure Baboon compiler has run and generated test files to '${generatedTestSourcesBaseDir.toAbsolutePath}'.")
      }
    }

    // TODO: Add a test to check if the generated test files can be compiled.
    // This would typically be covered by the sbt build process itself if the
    // generated files are part of the sbt test sources.
    // If sbt includes `generatedTestSourcesBaseDir` in `Test / sourceDirectories`,
    // then `sbt test` would compile them.
    // A specific test here could try to invoke scalac programmatically if needed,
    // but that's more involved.
  }
}
