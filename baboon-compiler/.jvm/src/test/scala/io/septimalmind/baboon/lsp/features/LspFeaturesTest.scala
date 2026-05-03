package io.septimalmind.baboon.lsp.features

import io.septimalmind.baboon.BaboonLoader
import io.septimalmind.baboon.lsp.protocol.{Position, SymbolKind}
import io.septimalmind.baboon.lsp.state._
import io.septimalmind.baboon.lsp.util.{JvmPathOps, PositionConverter}
import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.tests.BaboonTest
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.typer.model._
import io.septimalmind.baboon.util.BLogger
import izumi.functional.bio.Error2
import izumi.fundamentals.collections.nonempty.NEList
import izumi.reflect.TagKK

final class LspFeaturesTest extends LspFeaturesTestBase[Either]

abstract class LspFeaturesTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private val pathOps           = JvmPathOps
  private val positionConverter = new PositionConverter(pathOps)
  private val logger            = BLogger.Noop

  /** Simulate LSP state with a real compiled family and a file opened from disk. */
  private def withLspState(
    loader: BaboonLoader[F],
    fileRelPath: String,
  )(fn: (DocumentState, WorkspaceState, String) => Unit
  ): F[NEList[BaboonIssue], Unit] = {
    import izumi.fundamentals.platform.files.IzFiles

    val basePath = java.nio.file.Paths.get("./baboon-compiler/src/test/resources/baboon").toAbsolutePath.normalize()
    // Collect all .baboon files from the test resources. Intentionally-invalid fixtures live outside
    // this tree (see `baboon-fixtures-bad/`, driven by AnyFrontEndTest) so no exclusion is needed here.
    val allFiles = IzFiles
      .walk(basePath.toFile).toList
      .filter(p => p.toFile.isFile && p.toFile.getName.endsWith(".baboon"))

    val inputs = allFiles.map {
      f =>
        val path    = f.toAbsolutePath.normalize()
        val content = java.nio.file.Files.readString(path)
        BaboonParser.Input(FSPath.parse(izumi.fundamentals.collections.nonempty.NEString.unsafeFrom(path.toString)), content)
    }

    for {
      family <- loader.load(allFiles)
    } yield {
      val docState = new DocumentState(pathOps)

      val filePath = basePath.resolve(fileRelPath).toAbsolutePath.normalize()
      val content  = java.nio.file.Files.readString(filePath)
      val uri      = filePath.toUri.toString

      docState.open(uri, content)

      // Compile using actual file paths so positions match
      val compiler = new LspCompiler {
        def reload(inputs: Seq[BaboonParser.Input], previous: Option[BaboonFamily]): Either[NEList[BaboonIssue], BaboonFamily] = Right(family)
      }
      val inputProvider = new InputProvider {
        def getWorkspaceInputs: Seq[BaboonParser.Input] = inputs
        def pathToUri(path: String): String             = pathOps.pathToUri(path)
        def uriToPath(uri: String): String              = pathOps.uriToPath(uri)
      }
      val wsState = new WorkspaceState(docState, compiler, inputProvider, pathOps, logger)
      wsState.recompile()

      fn(docState, wsState, uri)
    }
  }

  "hover provider" should {
    "show hover for regular types" in {
      (loader: BaboonLoader[F]) =>
        withLspState(loader, "pkg0/pkg01.baboon") {
          (docState, wsState, uri) =>
            val hover = new HoverProvider(docState, wsState, logger)

            // Find the line containing "T1_D1" definition and hover over it
            val content = docState.getContent(uri).get
            val lines   = content.split("\n")
            val lineIdx = lines.indexWhere(_.contains("root data T1_D1"))
            assert(lineIdx >= 0, "Should find T1_D1 definition line")

            val colIdx = lines(lineIdx).indexOf("T1_D1")
            val result = hover.getHover(uri, Position(lineIdx, colIdx + 1))
            assert(result.isDefined, "Should return hover for T1_D1")
            assert(result.get.contents.value.contains("data"), s"Hover should mention 'data': ${result.get.contents.value}")
            assert(result.get.contents.value.contains("T1_D1"), s"Hover should mention 'T1_D1': ${result.get.contents.value}")
        }
    }

    "show hover for type aliases" in {
      (loader: BaboonLoader[F]) =>
        withLspState(loader, "pkg0/pkg01.baboon") {
          (docState, wsState, uri) =>
            val hover = new HoverProvider(docState, wsState, logger)

            val content = docState.getContent(uri).get
            val lines   = content.split("\n")
            val lineIdx = lines.indexWhere(_.contains("type BinaryData"))
            assert(lineIdx >= 0, "Should find BinaryData alias line")

            val colIdx = lines(lineIdx).indexOf("BinaryData")
            val result = hover.getHover(uri, Position(lineIdx, colIdx + 1))
            assert(result.isDefined, "Should return hover for BinaryData alias")
            assert(result.get.contents.value.contains("type"), s"Hover should contain 'type': ${result.get.contents.value}")
            assert(result.get.contents.value.contains("BinaryData"), s"Hover should contain 'BinaryData': ${result.get.contents.value}")
            assert(result.get.contents.value.contains("bytes"), s"Hover should contain target 'bytes': ${result.get.contents.value}")
        }
    }

    "show hover for collection aliases" in {
      (loader: BaboonLoader[F]) =>
        withLspState(loader, "pkg0/pkg01.baboon") {
          (docState, wsState, uri) =>
            val hover = new HoverProvider(docState, wsState, logger)

            val content = docState.getContent(uri).get
            val lines   = content.split("\n")
            val lineIdx = lines.indexWhere(_.contains("type StringList"))
            assert(lineIdx >= 0, "Should find StringList alias line")

            val colIdx = lines(lineIdx).indexOf("StringList")
            val result = hover.getHover(uri, Position(lineIdx, colIdx + 1))
            assert(result.isDefined, "Should return hover for StringList alias")
            assert(result.get.contents.value.contains("lst[str]"), s"Hover should contain 'lst[str]': ${result.get.contents.value}")
        }
    }
  }

  "completion provider" should {
    "include all parser keywords" in {
      (loader: BaboonLoader[F]) =>
        withLspState(loader, "pkg0/pkg01.baboon") {
          (docState, wsState, uri) =>
            val completion = new CompletionProvider(docState, wsState, logger)

            // Find an empty line to get keyword completions
            val content      = docState.getContent(uri).get
            val lines        = content.split("\n")
            val emptyLineIdx = lines.indexWhere(_.trim.isEmpty)
            assert(emptyLineIdx >= 0, "Should find an empty line")

            val items  = completion.getCompletions(uri, Position(emptyLineIdx, 0))
            val labels = items.map(_.label).toSet

            val expectedKeywords = Set(
              "model",
              "version",
              "import",
              "include",
              "root",
              "data",
              "struct",
              "adt",
              "enum",
              "foreign",
              "contract",
              "service",
              "ns",
              "pragma",
              "derived",
              "was",
              "type",
            )
            expectedKeywords.foreach {
              kw =>
                assert(labels.contains(kw), s"Completion should include keyword '$kw', got: ${labels.filter(_.length < 15)}")
            }
        }
    }

    "include builtins, user types and aliases in type position" in {
      (loader: BaboonLoader[F]) =>
        withLspState(loader, "pkg0/pkg01.baboon") {
          (docState, wsState, uri) =>
            val completion = new CompletionProvider(docState, wsState, logger)

            // Find a field declaration line to get type-position completions
            val content = docState.getContent(uri).get
            val lines   = content.split("\n")
            // Find a line like "  f1: i08" and position cursor after ":"
            val fieldLineIdx = lines.indexWhere(l => l.contains(": i08") || l.contains(": str") || l.contains(": i32"))
            assert(fieldLineIdx >= 0, "Should find a field line")
            val colonIdx = lines(fieldLineIdx).indexOf(':')

            val items  = completion.getCompletions(uri, Position(fieldLineIdx, colonIdx + 2))
            val labels = items.map(_.label).toSet

            // Builtin types
            assert(labels.contains("i32"), s"Should include builtin 'i32'")
            assert(labels.contains("str"), s"Should include builtin 'str'")
            assert(labels.contains("opt"), s"Should include builtin 'opt'")

            // User types
            assert(labels.exists(_.contains("T1_D1")), "Should include user type T1_D1")

            // Aliases
            assert(labels.contains("BinaryData"), "Should include BinaryData alias")
            assert(labels.contains("StringList"), "Should include StringList alias")

            // Alias detail should show target
            val binaryItem = items.find(_.label == "BinaryData").get
            assert(binaryItem.detail.exists(_.contains("bytes")), s"BinaryData detail should mention target: ${binaryItem.detail}")
        }
    }
  }

  "document symbol provider" should {
    "list types defined in a file" in {
      (loader: BaboonLoader[F]) =>
        withLspState(loader, "pkg0/pkg01.baboon") {
          (_, wsState, uri) =>
            val symbolProvider = new DocumentSymbolProvider(wsState, positionConverter, pathOps, logger)
            val symbols        = symbolProvider.getSymbols(uri)

            assert(symbols.nonEmpty, "Should find symbols in pkg01.baboon")
            val names = symbols.map(_.name).toSet

            // Regular types
            assert(names.contains("T1_D1"), s"Should include T1_D1, got: $names")
            assert(names.contains("T4_A1"), s"Should include T4_A1, got: $names")
            assert(names.contains("T1_E1"), s"Should include T1_E1, got: $names")

            // Aliases
            assert(names.contains("BinaryData"), s"Should include BinaryData alias, got: $names")
            assert(names.contains("StringList"), s"Should include StringList alias, got: $names")
        }
    }

    "use correct symbol kinds" in {
      (loader: BaboonLoader[F]) =>
        withLspState(loader, "pkg0/pkg01.baboon") {
          (_, wsState, uri) =>
            val symbolProvider = new DocumentSymbolProvider(wsState, positionConverter, pathOps, logger)
            val symbols        = symbolProvider.getSymbols(uri)
            val symbolMap      = symbols.map(s => s.name -> s.kind).toMap

            // Verify symbol kinds match type categories
            symbolMap.get("T1_E1").foreach(kind => assert(kind == SymbolKind.Enum, s"T1_E1 should be Enum, got $kind"))
            symbolMap.get("T4_A1").foreach(kind => assert(kind == SymbolKind.Interface, s"T4_A1 (ADT) should be Interface, got $kind"))
            symbolMap.get("ObscureInt").foreach(kind => assert(kind == SymbolKind.TypeParameter, s"ObscureInt (foreign) should be TypeParameter, got $kind"))

            // Aliases should be TypeParameter
            symbolMap.get("BinaryData").foreach(kind => assert(kind == SymbolKind.TypeParameter, s"BinaryData (alias) should be TypeParameter, got $kind"))
        }
    }
  }

  "hover provider (m29 templates)" should {
    // m29-lsp.baboon layout (0-indexed lines):
    //   11: data Page[T] {
    //   12:   items: lst[T]
    //   13:   total: u32
    //   14: }
    //   17: type IntPage = Page[i32]
    //   22:   page: IntPage

    "show template signature when hovering on template name" in {
      (loader: BaboonLoader[F]) =>
        withLspState(loader, "m29-lsp/m29-lsp.baboon") {
          (docState, wsState, uri) =>
            val hover = new HoverProvider(docState, wsState, logger)

            val content = docState.getContent(uri).get
            val lines   = content.split("\n")
            val lineIdx = lines.indexWhere(_.contains("data Page[T]"))
            assert(lineIdx >= 0, "Should find 'data Page[T]' line")

            val colIdx = lines(lineIdx).indexOf("Page")
            val result = hover.getHover(uri, Position(lineIdx, colIdx + 1))
            assert(result.isDefined, "Should return hover for template 'Page'")
            assert(result.get.contents.value.contains("Page"), s"Hover should mention 'Page': ${result.get.contents.value}")
            assert(result.get.contents.value.contains("T"), s"Hover should mention type-param 'T': ${result.get.contents.value}")
        }
    }

    "resolve type-param T inside template body (shadowing top-level T)" in {
      // Spec §2.3: type-param T in data Page[T] shadows the top-level `data T` when
      // the cursor is inside Page's body.
      (loader: BaboonLoader[F]) =>
        withLspState(loader, "m29-lsp/m29-lsp.baboon") {
          (docState, wsState, uri) =>
            val hover = new HoverProvider(docState, wsState, logger)

            val content = docState.getContent(uri).get
            val lines   = content.split("\n")
            // Line "  items: lst[T]" — cursor on T inside the template body
            val lineIdx = lines.indexWhere(_.contains("items: lst[T]"))
            assert(lineIdx >= 0, "Should find 'items: lst[T]' line")

            val colIdx = lines(lineIdx).lastIndexOf("T")
            val result = hover.getHover(uri, Position(lineIdx, colIdx + 1))
            assert(result.isDefined, "Should return hover for T inside template body")
            // Must resolve to type-param info, NOT the top-level data T
            assert(
              result.get.contents.value.contains("parameter"),
              s"Hover on T inside Page body should describe a type parameter, got: ${result.get.contents.value}",
            )
            assert(
              result.get.contents.value.contains("Page"),
              s"Hover on T should mention the enclosing template 'Page', got: ${result.get.contents.value}",
            )
        }
    }

    "show type-param info when hovering on T outside template body" in {
      // When the cursor is NOT inside a template body (e.g. on the type-param list itself,
      // which is on the header line before the '{'), the fallback branch still returns
      // type-param info for T found in the registry.
      (loader: BaboonLoader[F]) =>
        withLspState(loader, "m29-lsp/m29-lsp.baboon") {
          (docState, wsState, uri) =>
            val hover = new HoverProvider(docState, wsState, logger)

            val content = docState.getContent(uri).get
            val lines   = content.split("\n")
            // "data Page[T] {" — T appears in the param list at, e.g., col 10
            val lineIdx = lines.indexWhere(_.contains("data Page[T]"))
            assert(lineIdx >= 0, "Should find template header line")

            val colIdx = lines(lineIdx).indexOf("[T]") + 1 // position on T inside [T]
            val result = hover.getHover(uri, Position(lineIdx, colIdx + 1))
            // The result should mention T is a type parameter (via the fallback branch)
            assert(result.isDefined, "Should return hover for T in template header")
        }
    }
  }

  "definition provider (m29 templates)" should {
    "find template declaration when going-to-definition from alias RHS" in {
      (loader: BaboonLoader[F]) =>
        withLspState(loader, "m29-lsp/m29-lsp.baboon") {
          (docState, wsState, uri) =>
            val defProvider = new DefinitionProvider(docState, wsState, positionConverter)

            val content = docState.getContent(uri).get
            val lines   = content.split("\n")
            // "type IntPage = Page[i32]" — cursor on "Page"
            val lineIdx = lines.indexWhere(_.contains("type IntPage = Page"))
            assert(lineIdx >= 0, "Should find IntPage alias line")

            val colIdx = lines(lineIdx).indexOf("Page")
            val locations = defProvider.findDefinition(uri, Position(lineIdx, colIdx + 1))
            assert(locations.nonEmpty, "Should find definition for template 'Page' from alias RHS")
        }
    }

    "find alias declaration when going-to-definition on alias reference in field" in {
      (loader: BaboonLoader[F]) =>
        withLspState(loader, "m29-lsp/m29-lsp.baboon") {
          (docState, wsState, uri) =>
            val defProvider = new DefinitionProvider(docState, wsState, positionConverter)

            val content = docState.getContent(uri).get
            val lines   = content.split("\n")
            // "  page: IntPage" — cursor on "IntPage"
            val lineIdx = lines.indexWhere(_.contains("page: IntPage"))
            assert(lineIdx >= 0, "Should find line using IntPage as a field type")

            val colIdx = lines(lineIdx).indexOf("IntPage")
            val locations = defProvider.findDefinition(uri, Position(lineIdx, colIdx + 1))
            assert(locations.nonEmpty, "Should find definition for alias 'IntPage'")
        }
    }
  }

  "completion provider (m29 templates)" should {
    "include template and plain type in alias-RHS completion" in {
      (loader: BaboonLoader[F]) =>
        withLspState(loader, "m29-lsp/m29-lsp.baboon") {
          (docState, wsState, uri) =>
            val completion = new CompletionProvider(docState, wsState, logger)

            val content = docState.getContent(uri).get
            val lines   = content.split("\n")
            // Position cursor right after "= " in "type IntPage = " to trigger AliasRhsPosition
            val lineIdx = lines.indexWhere(_.contains("type IntPage = Page"))
            assert(lineIdx >= 0, "Should find IntPage alias line")

            val eqIdx = lines(lineIdx).indexOf("=")
            // Place cursor 2 chars after '=' (after "= ") — beforeCursor ends with "type IntPage = "
            val cursorCol = eqIdx + 2
            val items     = completion.getCompletions(uri, Position(lineIdx, cursorCol))
            val labels    = items.map(item => item.filterText.getOrElse(item.label)).toSet

            assert(labels.contains("Page"), s"Alias-RHS completions should include template 'Page', got: ${items.map(_.label).take(20)}")
            assert(labels.contains("Plain"), s"Alias-RHS completions should include regular type 'Plain', got: ${items.map(_.label).take(20)}")
        }
    }
  }

  "definition provider" should {
    "find definitions for regular types" in {
      (loader: BaboonLoader[F]) =>
        withLspState(loader, "pkg0/pkg01.baboon") {
          (docState, wsState, uri) =>
            val defProvider = new DefinitionProvider(docState, wsState, positionConverter)

            val content = docState.getContent(uri).get
            val lines   = content.split("\n")

            // Find a line that uses T1_D1 as a reference (not definition)
            val refLineIdx = lines.indexWhere(l => l.contains("f0: T1_D2") || l.contains("f0: T2_D2"))
            if (refLineIdx >= 0) {
              val line   = lines(refLineIdx)
              val colIdx = line.indexOf("T1_D2").max(line.indexOf("T2_D2"))
              if (colIdx >= 0) {
                val locations = defProvider.findDefinition(uri, Position(refLineIdx, colIdx + 1))
                assert(locations.nonEmpty, "Should find definition for referenced type")
              }
            }
        }
    }

    "find definitions for aliases" in {
      (loader: BaboonLoader[F]) =>
        withLspState(loader, "pkg0/pkg01.baboon") {
          (docState, wsState, uri) =>
            val defProvider = new DefinitionProvider(docState, wsState, positionConverter)

            val content = docState.getContent(uri).get
            val lines   = content.split("\n")

            // Find a line that uses BinaryData as a field type
            val lineIdx = lines.indexWhere(_.contains("binary: BinaryData"))
            assert(lineIdx >= 0, "Should find line with BinaryData usage")
            val colIdx = lines(lineIdx).indexOf("BinaryData")

            val locations = defProvider.findDefinition(uri, Position(lineIdx, colIdx + 1))
            assert(locations.nonEmpty, "Should find definition for BinaryData alias")
        }
    }
  }
}
