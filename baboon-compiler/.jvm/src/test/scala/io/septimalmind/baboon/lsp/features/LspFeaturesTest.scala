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

  /** Like `withLspState` but opens every good corpus file, for the corpus-wide robustness audit. */
  private def withLspStateAllFiles(
    loader: BaboonLoader[F]
  )(fn: (DocumentState, WorkspaceState, Seq[String]) => Unit
  ): F[NEList[BaboonIssue], Unit] = {
    import izumi.fundamentals.platform.files.IzFiles

    val basePath = java.nio.file.Paths.get("./baboon-compiler/src/test/resources/baboon").toAbsolutePath.normalize()
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
      val uris = allFiles.map {
        f =>
          val path    = f.toAbsolutePath.normalize()
          val content = java.nio.file.Files.readString(path)
          val uri     = path.toUri.toString
          docState.open(uri, content)
          uri
      }

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

      fn(docState, wsState, uris)
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

    "reject negative line in hover position" in {
      (loader: BaboonLoader[F]) =>
        withLspState(loader, "pkg0/pkg01.baboon") {
          (docState, wsState, uri) =>
            val hover = new HoverProvider(docState, wsState, logger)
            val result = hover.getHover(uri, Position(-1, 0))
            assert(result.isEmpty, "Hover at negative line should return None without throwing")
        }
    }

    "reject negative character in hover position" in {
      (loader: BaboonLoader[F]) =>
        withLspState(loader, "pkg0/pkg01.baboon") {
          (docState, wsState, uri) =>
            val hover = new HoverProvider(docState, wsState, logger)
            val result = hover.getHover(uri, Position(0, -1))
            assert(result.isEmpty, "Hover at negative character should return None without throwing")
        }
    }

    "reject both negative line and character in hover position" in {
      (loader: BaboonLoader[F]) =>
        withLspState(loader, "pkg0/pkg01.baboon") {
          (docState, wsState, uri) =>
            val hover = new HoverProvider(docState, wsState, logger)
            val result = hover.getHover(uri, Position(-1, -1))
            assert(result.isEmpty, "Hover at negative line and character should return None without throwing")
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
              "id",
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

    "include templates from templateRegistry (GAP-2)" in {
      // Templates are excised from domain.defs after monomorphisation and live in
      // domain.templateRegistry.templates.  Verify getSymbols surfaces them.
      (loader: BaboonLoader[F]) =>
        withLspState(loader, "m29-lsp/m29-lsp.baboon") {
          (_, wsState, uri) =>
            val symbolProvider = new DocumentSymbolProvider(wsState, positionConverter, pathOps, logger)
            val symbols        = symbolProvider.getSymbols(uri)
            val names          = symbols.map(_.name).toSet

            // Template defined in m29-lsp.baboon: data Page[T] { ... }
            assert(names.contains("Page"), s"Should include template 'Page', got: $names")
            // Regular non-template type in the same file must still appear
            assert(names.contains("Plain"), s"Should include regular type 'Plain', got: $names")
            // Verify template carries SymbolKind.Class
            val symbolMap = symbols.map(s => s.name -> s.kind).toMap
            symbolMap.get("Page").foreach(kind => assert(kind == SymbolKind.Class, s"Template 'Page' should have kind Class, got $kind"))
        }
    }

    // T35 / D15-item-6: regression for wrong-domain symbol construction.
    // DocumentSymbolProvider used to pass the FIRST domain's version (via `.head`) to every
    // createSymbol call, regardless of which domain the member actually belongs to.
    // For a file in a non-first domain that defines an ADT, this caused the branch children
    // to be resolved against the wrong domain's node map, yielding empty children.
    // Fix: allTypes now carries (domain, member) pairs; each member gets ITS OWN domain.
    "T35: ADT in non-first domain yields its branch children (wrong-domain regression)" in {
      // m30.sc.docs is a separate lineage from testpkg.pkg0 / my.ok.* / my.lsp.* — when the
      // whole corpus is loaded the family has 15+ lineages.  The pre-fix `.head` routes every
      // createSymbol call to the FIRST lineage's domain, so DocResult's branch TypeIds
      // (pkg=m30.sc.docs) are absent from that domain's node map and children come back empty.
      (loader: BaboonLoader[F]) =>
        withLspState(loader, "m30-sc-docs/m30_sc_docs.baboon") {
          (_, wsState, uri) =>
            val symbolProvider = new DocumentSymbolProvider(wsState, positionConverter, pathOps, logger)
            val symbols        = symbolProvider.getSymbols(uri)

            val symbolMap = symbols.map(s => s.name -> s).toMap
            assert(symbolMap.contains("DocResult"), s"Should include DocResult ADT, got: ${symbolMap.keySet}")

            val docResult = symbolMap("DocResult")
            val children  = docResult.children.getOrElse(Seq.empty)
            val childNames = children.map(_.name).toSet
            assert(
              childNames.contains("DocOk"),
              s"DocResult ADT should have child 'DocOk'; got children: $childNames — if empty, the wrong domain was used (D15/T35 regression)",
            )
            assert(
              childNames.contains("DocErr"),
              s"DocResult ADT should have child 'DocErr'; got children: $childNames — if empty, the wrong domain was used (D15/T35 regression)",
            )
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

    "NOT mis-shadow a top-level type referenced AFTER a template body closes" in {
      // Repro for the unbounded-backward-scan defect in HoverProvider.enclosingTemplateName:
      // the scan walks up line-by-line with no brace tracking, so it treats any line that lies
      // textually below a `data Name[T] { … }` header — even past the closing `}` — as "inside"
      // that template body. Consequence: a reference to a top-level type whose name collides with
      // a type-param (here `T`) resolves to "type parameter of template Page" instead of the
      // top-level `data T`. We drive a synthetic document against the real m29-lsp family (which
      // defines both `data T` and `data Page[T]`).
      (loader: BaboonLoader[F]) =>
        withLspState(loader, "m29-lsp/m29-lsp.baboon") {
          (docState, wsState, realUri) =>
            val hover = new HoverProvider(docState, wsState, logger)

            // Sanity: top-level `data T` is reachable (User.marker: T) so it resolves on its own.
            val realContent = docState.getContent(realUri).get
            val realLines   = realContent.split("\n")
            val markerIdx   = realLines.indexWhere(_.contains("marker: T"))
            assert(markerIdx >= 0, "Should find 'marker: T' line in m29-lsp.baboon")
            val markerCol = realLines(markerIdx).lastIndexOf("T")
            val realT     = hover.getHover(realUri, Position(markerIdx, markerCol + 1))
            assert(realT.isDefined, "Hover on User.marker: T should resolve")
            assert(realT.get.contents.value.contains("data"), s"Top-level data T should be reachable/resolvable, got: ${realT.get.contents.value}")

            // Synthetic doc: a closed template body, then an unrelated DTO referencing top-level T.
            val synthUri = "file:///synthetic-shadowing.baboon"
            val synth =
              """data Page[T] {
                |  items: lst[T]
                |}
                |
                |data Holder {
                |  ref: T
                |}
                |""".stripMargin
            docState.open(synthUri, synth)

            val lines   = synth.split("\n", -1)
            val lineIdx = lines.indexWhere(_.contains("ref: T"))
            assert(lineIdx >= 0, "Should find 'ref: T' line")
            val colIdx = lines(lineIdx).lastIndexOf("T")
            val result = hover.getHover(synthUri, Position(lineIdx, colIdx + 1))

            assert(result.isDefined, "Should return hover for T referenced in Holder")
            val md = result.get.contents.value
            // T here is the top-level `data T`, NOT Page's type-param: the cursor is past Page's `}`.
            assert(
              !md.contains("parameter"),
              s"Hover on T in Holder (after Page's body closed) must NOT report a type parameter, got: $md",
            )
            assert(
              md.contains("data"),
              s"Hover on T in Holder should resolve to top-level `data T`, got: $md",
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

            val colIdx    = lines(lineIdx).indexOf("Page")
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

            val colIdx    = lines(lineIdx).indexOf("IntPage")
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

    "PR-29.15: alias-RHS completion at empty prefix on NsIntPage line surfaces NsPage (AliasRhsPosition, unfiltered)" in {
      // Regression guard for the un-prefixed case: cursor is right after "= ", so beforeCursor
      // ends with "type NsIntPage = " and the captured prefix is empty. Both old \w* and new
      // [\w.]* regexes match empty identically, so this test proves AliasRhsPosition fires and
      // all templates are returned unfiltered — NOT that [\w.]* is exercised.
      (loader: BaboonLoader[F]) =>
        withLspState(loader, "m29-lsp/m29-lsp.baboon") {
          (docState, wsState, uri) =>
            val completion = new CompletionProvider(docState, wsState, logger)

            val content = docState.getContent(uri).get
            val lines   = content.split("\n")
            // Line: "type NsIntPage = nstemplate.NsPage[i32]"
            val lineIdx = lines.indexWhere(_.contains("type NsIntPage = nstemplate.NsPage"))
            assert(lineIdx >= 0, "Should find NsIntPage alias line")

            val eqIdx     = lines(lineIdx).indexOf("=")
            val cursorCol = eqIdx + 2 // cursor after "= "; captured prefix is empty
            val items     = completion.getCompletions(uri, Position(lineIdx, cursorCol))

            val labels = items.map(item => item.filterText.getOrElse(item.label)).toSet
            assert(
              labels.contains("NsPage"),
              s"Alias-RHS completions at empty prefix should include template 'NsPage', got: ${items.map(_.label).take(20)}",
            )
        }
    }

    "PR-29.15: alias-RHS completion with qualified prefix 'nstemplate.' does not leak keywords ([\\w.]* regex exercises the dot)" in {
      // Exercises the [\w.]* regex with a dot-qualified prefix.
      //
      // Line: "type NsIntPage = nstemplate.NsPage[i32]"
      // Cursor is placed right after the dot in "nstemplate." so that
      //   beforeCursor = "type NsIntPage = nstemplate."
      //
      // Old \w* regex: "nstemplate." contains a dot, so (\w*)$ cannot consume the entire suffix
      //   starting at "nstemplate." — the aliasRhsPattern match fails entirely, the context
      //   falls through to CompletionContext.Unknown with empty prefix, and ALL items (including
      //   keywords like "ns") are returned.
      //
      // New [\w.]* regex: "nstemplate." is consumed by ([\w.]*)$, the pattern matches with
      //   prefix = "nstemplate.", AliasRhsPosition fires. Keywords are NOT included in the
      //   candidate set for AliasRhsPosition, so "ns" must be absent from results.
      //
      // Therefore: the absence of keyword "ns" in the results distinguishes the two regexes.
      (loader: BaboonLoader[F]) =>
        withLspState(loader, "m29-lsp/m29-lsp.baboon") {
          (docState, wsState, uri) =>
            val completion = new CompletionProvider(docState, wsState, logger)

            val content = docState.getContent(uri).get
            val lines   = content.split("\n")
            val lineIdx = lines.indexWhere(_.contains("type NsIntPage = nstemplate.NsPage"))
            assert(lineIdx >= 0, "Should find NsIntPage alias line")

            val line   = lines(lineIdx)
            val dotIdx = line.indexOf("nstemplate.")
            assert(dotIdx >= 0, "Should find 'nstemplate.' in the alias line")
            // cursor right after the dot — beforeCursor = "type NsIntPage = nstemplate."
            val cursorCol = dotIdx + "nstemplate.".length
            val items     = completion.getCompletions(uri, Position(lineIdx, cursorCol))

            // With AliasRhsPosition the candidate set is types + builtins + templates (no keywords).
            // The keyword "ns" would appear only if CompletionContext.Unknown fired (old \w* regression).
            val labels = items.map(item => item.filterText.getOrElse(item.label)).toSet
            assert(
              !labels.contains("ns"),
              s"Keyword 'ns' must be absent in AliasRhsPosition (its presence means Unknown fired, i.e. old \\w* regression), got: ${items.map(_.label).take(30)}",
            )
        }
    }
  }

  "hover provider (m33 structural-arm template)" should {
    // m33-lsp.baboon layout (1-indexed lines):
    //   10: data MyGen[T] { v: T }
    //   12: root data Holder {
    //   13:   + MyGen[i32]
    //   14: }
    //
    // Smoke: hover on `MyGen` inside `+ MyGen[i32]` returns the template signature
    // (looked up via domain.templateRegistry — templates are excised from domain.defs
    // post-monomorphisation, see PR-29.7 / GAP-2 / DocumentSymbolProvider).
    "show template signature when hovering on template name in + arm" in {
      (loader: BaboonLoader[F]) =>
        withLspState(loader, "m33-lsp/m33-lsp.baboon") {
          (docState, wsState, uri) =>
            val hover = new HoverProvider(docState, wsState, logger)

            val content = docState.getContent(uri).get
            val lines   = content.split("\n")
            val lineIdx = lines.indexWhere(_.contains("+ MyGen[i32]"))
            assert(lineIdx >= 0, "Should find '+ MyGen[i32]' line")

            val colIdx = lines(lineIdx).indexOf("MyGen")
            val result = hover.getHover(uri, Position(lineIdx, colIdx + 1))
            assert(result.isDefined, "Should return hover for template 'MyGen' in + arm position")
            val markdown = result.get.contents.value
            assert(markdown.contains("MyGen"), s"Hover should mention 'MyGen': $markdown")
            // Template hover renders header `data MyGen[T] { … }` plus a "Template" label.
            assert(markdown.contains("Template"), s"Hover should describe a template: $markdown")
            assert(markdown.contains("[T]"), s"Hover should list type-param '[T]': $markdown")
        }
    }
  }

  "completion provider (m33 structural-arm position)" should {
    // Smoke: completion right after `+ ` inside a DTO body offers the template `MyGen`
    // alongside concrete types and builtins (CompletionContext.StructuralArmPosition).
    "include template in candidate list right after '+ ' in DTO body" in {
      (loader: BaboonLoader[F]) =>
        withLspState(loader, "m33-lsp/m33-lsp.baboon") {
          (docState, wsState, uri) =>
            val completion = new CompletionProvider(docState, wsState, logger)

            val content = docState.getContent(uri).get
            val lines   = content.split("\n")
            // Line: "data Holder { + MyGen[i32] }" — place cursor right after "+ "
            val lineIdx = lines.indexWhere(_.contains("+ MyGen[i32]"))
            assert(lineIdx >= 0, "Should find '+ MyGen[i32]' line")

            val plusIdx   = lines(lineIdx).indexOf('+')
            val cursorCol = plusIdx + 2 // right after "+ "; captured prefix is empty
            val items     = completion.getCompletions(uri, Position(lineIdx, cursorCol))
            val labels    = items.map(item => item.filterText.getOrElse(item.label)).toSet

            assert(
              labels.contains("MyGen"),
              s"Structural-arm completions should include template 'MyGen', got: ${items.map(_.label).take(20)}",
            )
            // Concrete types + builtins must also surface (the head can be a concrete type too).
            assert(
              labels.contains("i32"),
              s"Structural-arm completions should include builtin 'i32', got: ${items.map(_.label).take(20)}",
            )
            // Keywords are NOT valid as a structural-arm head — `data` etc. must be absent.
            assert(
              !labels.contains("data"),
              s"Keyword 'data' must be absent in structural-arm position, got: ${items.map(_.label).take(30)}",
            )
        }
    }
  }

  "hover provider (m30 docs)" should {
    // m30_sc_docs.baboon contains doc-bearing types:
    //   /** A simple item with field-level docs. */
    //   data DocItem { ... }
    //
    //   /** The CRUD service. */
    //   root service DocCrud {
    //     /** Create an item. */
    //     def create (DocReq): DocResp
    //   }

    "include type-level doc in hover for a doc-bearing type" in {
      (loader: BaboonLoader[F]) =>
        withLspState(loader, "m30-sc-docs/m30_sc_docs.baboon") {
          (docState, wsState, uri) =>
            val hover = new HoverProvider(docState, wsState, logger)

            val content = docState.getContent(uri).get
            val lines   = content.split("\n")
            // Find "data DocItem {" — hover over "DocItem"
            val lineIdx = lines.indexWhere(_.contains("data DocItem"))
            assert(lineIdx >= 0, "Should find 'data DocItem' line")

            val colIdx = lines(lineIdx).indexOf("DocItem")
            val result = hover.getHover(uri, Position(lineIdx, colIdx + 1))
            assert(result.isDefined, "Should return hover for DocItem")
            val markdown = result.get.contents.value
            assert(markdown.contains("DocItem"), s"Hover should mention 'DocItem': $markdown")
            // Type-level doc text must appear in the hover output
            assert(
              markdown.contains("A simple item with field-level docs"),
              s"Hover should include type-level doc: $markdown",
            )
        }
    }

    "include field-level doc in hover for a type with documented fields" in {
      (loader: BaboonLoader[F]) =>
        withLspState(loader, "m30-sc-docs/m30_sc_docs.baboon") {
          (docState, wsState, uri) =>
            val hover = new HoverProvider(docState, wsState, logger)

            val content = docState.getContent(uri).get
            val lines   = content.split("\n")
            // Hover over "DocItem" — the rendered output must include field-level docs
            val lineIdx = lines.indexWhere(_.contains("data DocItem"))
            assert(lineIdx >= 0, "Should find 'data DocItem' line")

            val colIdx = lines(lineIdx).indexOf("DocItem")
            val result = hover.getHover(uri, Position(lineIdx, colIdx + 1))
            assert(result.isDefined, "Should return hover for DocItem")
            val markdown = result.get.contents.value
            // "name" field has doc "Display name of the item."
            assert(
              markdown.contains("Display name of the item"),
              s"Hover should include field doc for 'name': $markdown",
            )
            // "price" field has suffix doc "never negative"
            assert(
              markdown.contains("never negative"),
              s"Hover should include suffix field doc for 'price': $markdown",
            )
        }
    }
  }

  "generics provider coverage (m29-ok: adt + service templates)" should {
    // m29.baboon (1-indexed) — adt + service templates NOT covered by the m29-lsp fixture:
    //   12: data Page[T] { … }
    //   18: adt Envelope[T, E] {
    //   19:   data Ok  { value: T }
    //   20:   data Err { error: E }
    //   35: service Crud[K, V] {
    //   36:   def get (K): V
    //   38: root type IntStrEnvelope = Envelope[i32, str] : derived[json], derived[ueba]
    //   40: root type IntStrCrud = Crud[i32, str]

    def hoverOnWord(docState: DocumentState, wsState: WorkspaceState, uri: String, lineSubstr: String, word: String) = {
      val hover   = new HoverProvider(docState, wsState, logger)
      val content = docState.getContent(uri).get
      val lines   = content.split("\n")
      val lineIdx = lines.indexWhere(_.contains(lineSubstr))
      assert(lineIdx >= 0, s"Should find line containing '$lineSubstr'")
      val colIdx = lines(lineIdx).indexOf(word)
      assert(colIdx >= 0, s"Should find '$word' on line '$lineSubstr'")
      hover.getHover(uri, Position(lineIdx, colIdx + 1))
    }

    "show template signature when hovering on an adt template name" in {
      (loader: BaboonLoader[F]) =>
        withLspState(loader, "m29-ok/m29.baboon") {
          (docState, wsState, uri) =>
            val result = hoverOnWord(docState, wsState, uri, "adt Envelope[T, E]", "Envelope")
            assert(result.isDefined, "Should return hover for adt template 'Envelope'")
            val md = result.get.contents.value
            assert(md.contains("adt"), s"Hover should render kind 'adt': $md")
            assert(md.contains("Envelope"), s"Hover should mention 'Envelope': $md")
            assert(md.contains("Template"), s"Hover should label it a Template: $md")
        }
    }

    "show template signature when hovering on a service template name" in {
      (loader: BaboonLoader[F]) =>
        withLspState(loader, "m29-ok/m29.baboon") {
          (docState, wsState, uri) =>
            val result = hoverOnWord(docState, wsState, uri, "service Crud[K, V]", "Crud")
            assert(result.isDefined, "Should return hover for service template 'Crud'")
            val md = result.get.contents.value
            assert(md.contains("service"), s"Hover should render kind 'service': $md")
            assert(md.contains("Crud"), s"Hover should mention 'Crud': $md")
            assert(md.contains("Template"), s"Hover should label it a Template: $md")
        }
    }

    "resolve a type-param inside an adt template body" in {
      (loader: BaboonLoader[F]) =>
        withLspState(loader, "m29-ok/m29.baboon") {
          (docState, wsState, uri) =>
            // `data Ok  { value: T }` — T is a type-param of the enclosing adt Envelope[T, E].
            val result = hoverOnWord(docState, wsState, uri, "data Ok  { value: T }", "T")
            assert(result.isDefined, "Should return hover for type-param T inside adt body")
            assert(
              result.get.contents.value.contains("parameter"),
              s"Hover on T inside Envelope body should describe a type parameter, got: ${result.get.contents.value}",
            )
        }
    }

    "include adt and service templates in document symbols" in {
      (loader: BaboonLoader[F]) =>
        withLspState(loader, "m29-ok/m29.baboon") {
          (_, wsState, uri) =>
            val symbolProvider = new DocumentSymbolProvider(wsState, positionConverter, pathOps, logger)
            val names          = symbolProvider.getSymbols(uri).map(_.name).toSet
            assert(names.contains("Envelope"), s"Document symbols should include adt template 'Envelope', got: $names")
            assert(names.contains("Crud"), s"Document symbols should include service template 'Crud', got: $names")
            assert(names.contains("Page"), s"Document symbols should include data template 'Page', got: $names")
        }
    }

    "find definition of an adt template from its alias RHS" in {
      (loader: BaboonLoader[F]) =>
        withLspState(loader, "m29-ok/m29.baboon") {
          (docState, wsState, uri) =>
            val defProvider = new DefinitionProvider(docState, wsState, positionConverter)
            val content     = docState.getContent(uri).get
            val lines       = content.split("\n")
            val lineIdx     = lines.indexWhere(_.contains("type IntStrEnvelope = Envelope"))
            assert(lineIdx >= 0, "Should find IntStrEnvelope alias line")
            val colIdx    = lines(lineIdx).indexOf("Envelope", lines(lineIdx).indexOf("="))
            val locations = defProvider.findDefinition(uri, Position(lineIdx, colIdx + 1))
            assert(locations.nonEmpty, "Should find definition for adt template 'Envelope' from alias RHS")
        }
    }

    "find definition of a service template from its alias RHS" in {
      (loader: BaboonLoader[F]) =>
        withLspState(loader, "m29-ok/m29.baboon") {
          (docState, wsState, uri) =>
            val defProvider = new DefinitionProvider(docState, wsState, positionConverter)
            val content     = docState.getContent(uri).get
            val lines       = content.split("\n")
            val lineIdx     = lines.indexWhere(_.contains("type IntStrCrud = Crud"))
            assert(lineIdx >= 0, "Should find IntStrCrud alias line")
            val colIdx    = lines(lineIdx).indexOf("Crud", lines(lineIdx).indexOf("="))
            val locations = defProvider.findDefinition(uri, Position(lineIdx, colIdx + 1))
            assert(locations.nonEmpty, "Should find definition for service template 'Crud' from alias RHS")
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

  "provider robustness (Part B feature audit)" should {
    // Drive every provider over every word/line of every good corpus file. The corpus exercises
    // every language feature (DTO/ADT/enum/foreign/contract/service/alias/template/ns/collections/
    // any/identifier/@root/derived/structural-ops/multi-version). A provider must never throw on a
    // successfully-compiled model — at worst it returns None/empty.
    "never throw on any word/line of any good corpus file" in {
      (loader: BaboonLoader[F]) =>
        withLspStateAllFiles(loader) {
          (docState, wsState, uris) =>
            val hover      = new HoverProvider(docState, wsState, logger)
            val defs       = new DefinitionProvider(docState, wsState, positionConverter)
            val completion = new CompletionProvider(docState, wsState, logger)
            val symbols    = new DocumentSymbolProvider(wsState, positionConverter, pathOps, logger)

            uris.foreach {
              uri =>
                // Document symbols must not throw for any file.
                symbols.getSymbols(uri)

                val content = docState.getContent(uri).getOrElse("")
                val lines   = content.split("\n", -1)
                lines.indices.foreach {
                  lineIdx =>
                    val line = lines(lineIdx)
                    // Completion at start, after every ':' / '[' / '=' / '+' and at end of line.
                    val cols = (0 to line.length).toList
                    cols.foreach {
                      col =>
                        completion.getCompletions(uri, Position(lineIdx, col))
                        hover.getHover(uri, Position(lineIdx, col))
                        defs.findDefinition(uri, Position(lineIdx, col))
                    }
                }
            }
            assert(uris.nonEmpty, "Corpus should contain at least one file")
        }
    }
  }

  // T33: guard URI-to-path conversion at LSP request boundaries
  "URI guard (T33/D15-5a)" should {
    // Providers must not throw for untitled:, malformed, or non-file URIs.
    // documentSymbol must return empty (it calls uriToPath directly).
    // hover and definition return None/empty because they look up content by URI key — safe.
    // completion returns workspace-level keyword/type completions even for unknown URIs
    // (correct behaviour — no uriToPath call) so we only assert NO throw for completion.

    def checkNoThrow(
      docState: DocumentState,
      wsState: WorkspaceState,
      uri: String,
      label: String,
    ): Unit = {
      val hover      = new HoverProvider(docState, wsState, logger)
      val defs       = new DefinitionProvider(docState, wsState, positionConverter)
      val completion = new CompletionProvider(docState, wsState, logger)
      val symbols    = new DocumentSymbolProvider(wsState, positionConverter, pathOps, logger)

      val pos = Position(0, 0)

      // documentSymbol: guard intercepts uriToPath — must return empty Seq, not throw.
      val symResult = symbols.getSymbols(uri)
      assert(symResult.isEmpty, s"documentSymbol($label): expected empty Seq, got $symResult")

      // hover and definition: content lookup by URI key returns None safely — must return None/empty.
      val hoverResult = hover.getHover(uri, pos)
      assert(hoverResult.isEmpty, s"hover($label): expected None, got $hoverResult")

      val defResult = defs.findDefinition(uri, pos)
      assert(defResult.isEmpty, s"definition($label): expected empty Seq, got $defResult")

      // completion: no uriToPath call; content lookup returns None; returns workspace completions.
      // Assert only no-throw (invoking getCompletions must not propagate any exception).
      completion.getCompletions(uri, pos) // no assertion on result, only no-throw
    }

    "return empty results for untitled: URIs without throwing" in {
      (loader: BaboonLoader[F]) =>
        withLspState(loader, "pkg0/pkg01.baboon") {
          (docState, wsState, _) =>
            checkNoThrow(docState, wsState, "untitled:Untitled-1", "untitled:")
        }
    }

    "return empty results for syntactically malformed URIs without throwing" in {
      (loader: BaboonLoader[F]) =>
        withLspState(loader, "pkg0/pkg01.baboon") {
          (docState, wsState, _) =>
            // file://%zz is syntactically invalid (bad percent-encoding)
            checkNoThrow(docState, wsState, "file://%zz", "malformed")
        }
    }

    "return empty results for non-file scheme URIs without throwing" in {
      (loader: BaboonLoader[F]) =>
        withLspState(loader, "pkg0/pkg01.baboon") {
          (docState, wsState, _) =>
            checkNoThrow(docState, wsState, "vscode-userdata:/settings.json", "vscode-userdata:")
        }
    }

    // Additional assertion: documentSymbol with an untitled URI returns a well-formed empty array
    // (not an error response) — the guard must complete the method call normally, yielding Seq.empty.
    "documentSymbol returns well-formed empty Seq (not a throw) for untitled: URI" in {
      (loader: BaboonLoader[F]) =>
        withLspState(loader, "pkg0/pkg01.baboon") {
          (_, wsState, _) =>
            val symbols = new DocumentSymbolProvider(wsState, positionConverter, pathOps, logger)
            val result  = symbols.getSymbols("untitled:Untitled-1")
            // Seq.empty is a well-formed result; an exception would have propagated.
            assert(result == Seq.empty, s"Expected Seq.empty for untitled URI, got $result")
        }
    }
  }
}
