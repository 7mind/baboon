package io.septimalmind.baboon.tests

import io.circe.Json
import io.septimalmind.baboon.BaboonLoader
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TranslationIssue}
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.typer.model.{Pkg, Version}
import io.septimalmind.baboon.typer.BaboonRuntimeCodec
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.exceptions.Issue
import izumi.fundamentals.platform.language.SourceFilePosition
import izumi.reflect.TagKK

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.*

final class RTCodecTest extends RTCodecTestBase[Either]

abstract class RTCodecTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {
  "baboon converter should" should {
    "read baboon binaries" in {
      (loader: BaboonLoader[F], codec: BaboonRuntimeCodec[F]) =>
        for {
          fam       <- loadPkg(loader)
          data: Json = io.circe.parser.parse("""{"B2":{"f":135983116}}""").toOption.get
          encoded   <- codec.encode(fam, Pkg(NEList("testpkg", "pkg0")), Version.parse("3.0.0"), "testpkg.pkg0/:#T5_A1", data, indexed = false)
          decoded   <- codec.decode(fam, Pkg(NEList("testpkg", "pkg0")), Version.parse("3.0.0"), "testpkg.pkg0/:#T5_A1", encoded)
        } yield {
          assert(data == decoded)
        }
    }

    "roundtrip foreign types with rt binding through UEBA" in {
      (loader: BaboonLoader[F], codec: BaboonRuntimeCodec[F]) =>
        for {
          fam <- loadPkg(loader)
          // T1_Foreign_DTO has ft: ObscureInt and fm: map[ObscureInt, ObscureInt]
          // ObscureInt has rt = i32, so it should encode/decode as i32
          data: Json = io.circe.parser.parse("""{"ft":42,"fm":{"123":456,"789":0}}""").toOption.get
          encoded   <- codec.encode(fam, Pkg(NEList("testpkg", "pkg0")), Version.parse("1.0.0"), "testpkg.pkg0/:#T1_Foreign_DTO", data, indexed = false)
          decoded   <- codec.decode(fam, Pkg(NEList("testpkg", "pkg0")), Version.parse("1.0.0"), "testpkg.pkg0/:#T1_Foreign_DTO", encoded)
        } yield {
          assert(data == decoded)
        }
    }

    "roundtrip foreign types with rt binding through indexed UEBA" in {
      (loader: BaboonLoader[F], codec: BaboonRuntimeCodec[F]) =>
        for {
          fam       <- loadPkg(loader)
          data: Json = io.circe.parser.parse("""{"ft":42,"fm":{"123":456,"789":0}}""").toOption.get
          encoded   <- codec.encode(fam, Pkg(NEList("testpkg", "pkg0")), Version.parse("1.0.0"), "testpkg.pkg0/:#T1_Foreign_DTO", data, indexed = true)
          decoded   <- codec.decode(fam, Pkg(NEList("testpkg", "pkg0")), Version.parse("1.0.0"), "testpkg.pkg0/:#T1_Foreign_DTO", encoded)
        } yield {
          assert(data == decoded)
        }
    }

    "roundtrip JSON files through UEBA" in {
      (loader: BaboonLoader[F], codec: BaboonRuntimeCodec[F]) =>
        val metaPath = Paths.get("target/test-regular/cs-stub/BaboonDefinitions/Generated/baboon-meta.json")

        (if (!Files.exists(metaPath)) {
           F.pure { assume(false, "baboon-meta.json not found"); () }
         } else {
           for {
             fam <- loadPkg(loader)

             // Read and parse baboon-meta.json
             metaJson <- F.fromEither {
               val metaContent = Files.readString(metaPath)
               io.circe.parser.parse(metaContent).left.map(e => new RuntimeException(s"Failed to parse baboon-meta.json: ${e.getMessage}"))
             }

             // Extract identifiers for version 3.0.0
             typeIdMap: Map[String, String] <- F.fromOption(new RuntimeException("Failed to extract identifiers from baboon-meta.json")) {
               for {
                 identifiers <- metaJson.hcursor.downField("identifiers").focus
                 pkg         <- identifiers.hcursor.downField("testpkg.pkg0").focus
                 version     <- pkg.hcursor.downField("3.0.0").focus.flatMap(_.asObject)
               } yield {
                 // Invert the mapping: filename -> typeId
                 version.toMap.map {
                   case (typeId, fileNameJson) =>
                     fileNameJson.asString.getOrElse("") -> typeId
                 }
               }
             }

             // Process JSON files
             jsonDir: java.nio.file.Path = Paths.get("target/test-regular/target/cs/json-default")
             _ <- F.fromEither {
               if (!Files.exists(jsonDir)) {
                 Left(new RuntimeException(s"JSON test directory not found: $jsonDir"))
               } else {
                 Right(())
               }
             }

             jsonFiles: List[java.nio.file.Path] = Files.list(jsonDir).iterator().asScala.toList.filter(_.toString.endsWith(".json"))

             _ <- F.traverse(jsonFiles) {
               jsonFile =>
                 val fileName = jsonFile.getFileName.toString.stripSuffix(".json")

                 typeIdMap.get(fileName) match {
                   case Some(typeId) =>
                     for {
                       // Read JSON file
                       jsonContent <- F.fromEither {
                         val content = Files.readString(jsonFile)
                         io.circe.parser.parse(content).left.map(e => new RuntimeException(s"Failed to parse $jsonFile: ${e.getMessage}"))
                       }

                       // Encode to UEBA
                       uebaBytes <- codec.encode(fam, Pkg(NEList("testpkg", "pkg0")), Version.parse("3.0.0"), typeId, jsonContent, indexed = false)

                       // Decode back to JSON
                       decodedJson <- codec.decode(fam, Pkg(NEList("testpkg", "pkg0")), Version.parse("3.0.0"), typeId, uebaBytes)

                       // Compare
                       _ <- F.fromEither {
                         if (jsonContent == decodedJson) {
                           Right(())
                         } else {
                           Left(new RuntimeException(s"Roundtrip failed for $fileName:\nExpected: $jsonContent\nGot: $decodedJson"))
                         }
                       }
                     } yield ()

                   case None =>
                     // Skip files without type ID mapping
                     F.unit
                 }
             }
           } yield {
             val _ = assert(jsonFiles.nonEmpty, "No JSON files found to test")
           }
         }): F[Any, Unit]
    }

    def testUebaRoundtrip(loader: BaboonLoader[F], codec: BaboonRuntimeCodec[F], uebaDir: String, indexed: Boolean) = {
      val metaPath = Paths.get("target/test-regular/cs-stub/BaboonDefinitions/Generated/baboon-meta.json")

      for {
        fam <- loadPkg(loader)

        // Read and parse baboon-meta.json
        metaJson <- F.fromEither {
          val metaContent = Files.readString(metaPath)
          io.circe.parser.parse(metaContent).left.map(_ => new RuntimeException("Failed to parse baboon-meta.json"))
        }

        // Extract identifiers for version 3.0.0
        typeIdMap: Map[String, String] <- F.fromOption(new RuntimeException("Failed to extract identifiers from baboon-meta.json")) {
          for {
            identifiers <- metaJson.hcursor.downField("identifiers").focus
            pkg         <- identifiers.hcursor.downField("testpkg.pkg0").focus
            version     <- pkg.hcursor.downField("3.0.0").focus.flatMap(_.asObject)
          } yield {
            // Invert the mapping: filename -> typeId
            version.toMap.map {
              case (typeId, fileNameJson) =>
                fileNameJson.asString.getOrElse("") -> typeId
            }
          }
        }

        // Process UEBA files
        uebaDirPath: java.nio.file.Path = Paths.get(uebaDir)

        // Check if directory exists, skip test if not
        uebaFiles: List[java.nio.file.Path] =
          if (!Files.exists(uebaDirPath)) {
            List.empty
          } else {
            Files.list(uebaDirPath).iterator().asScala.toList.filter(_.toString.endsWith(".uebin"))
          }

        _ <- F.traverse(uebaFiles) {
          uebaFile =>
            val fileName = uebaFile.getFileName.toString.stripSuffix(".uebin")

            typeIdMap.get(fileName) match {
              case Some(typeId) =>
                for {
                  // Read UEBA file
                  uebaBytes <- F.pure {
                    Vector.from(Files.readAllBytes(uebaFile))
                  }

                  // Decode to JSON
                  jsonContent <- codec.decode(fam, Pkg(NEList("testpkg", "pkg0")), Version.parse("3.0.0"), typeId, uebaBytes).leftMap {
                    err =>
                      BaboonIssue.Translation(
                        TranslationIssue
                          .TranslationBug()(Issue.IssueContext(SourceFilePosition.unknown, new RuntimeException(s"Decode failed for $fileName (typeId=$typeId): $err")))
                      )
                  }

                  // Encode back to UEBA
                  reEncodedBytes <- codec.encode(fam, Pkg(NEList("testpkg", "pkg0")), Version.parse("3.0.0"), typeId, jsonContent, indexed).leftMap {
                    err =>
                      BaboonIssue.Translation(
                        TranslationIssue
                          .TranslationBug()(Issue.IssueContext(SourceFilePosition.unknown, new RuntimeException(s"Encode failed for $fileName (typeId=$typeId): $err")))
                      )
                  }

                  // Compare
                  _ <- F.fromEither {
                    if (uebaBytes == reEncodedBytes) {
                      Right(())
                    } else {
                      Left(new RuntimeException(s"Roundtrip failed for $fileName: byte arrays differ"))
                    }
                  }
                } yield ()

              case None =>
                // Skip files without type ID mapping
                F.unit
            }
        }
      } yield {
        // If no files found, it's okay (directory might not exist or be empty)
        ()
      }
    }

    "roundtrip compact UEBA files through JSON" in {
      (loader: BaboonLoader[F], codec: BaboonRuntimeCodec[F]) =>
        val metaPath = Paths.get("target/test-regular/cs-stub/BaboonDefinitions/Generated/baboon-meta.json")

        (if (!Files.exists(metaPath)) {
           F.pure { assume(false, "baboon-meta.json not found"); () }
         } else {
           testUebaRoundtrip(loader, codec, "target/test-regular/target/cs/ueba-compact", indexed = false)
         }): F[Any, Unit]
    }

    "roundtrip indexed UEBA files through JSON" in {
      (loader: BaboonLoader[F], codec: BaboonRuntimeCodec[F]) =>
        val metaPath = Paths.get("target/test-regular/cs-stub/BaboonDefinitions/Generated/baboon-meta.json")

        (if (!Files.exists(metaPath)) {
           F.pure { assume(false, "baboon-meta.json not found"); () }
         } else {
           testUebaRoundtrip(loader, codec, "target/test-regular/target/cs/ueba-indexed", indexed = true)
         }): F[Any, Unit]
    }
  }

}
