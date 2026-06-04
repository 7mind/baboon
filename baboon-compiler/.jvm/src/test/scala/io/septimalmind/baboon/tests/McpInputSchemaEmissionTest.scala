package io.septimalmind.baboon.tests

import com.networknt.schema.{InputFormat, JsonSchemaFactory, SchemaValidatorsConfig, SpecVersion}
import io.circe.Json
import io.septimalmind.baboon.BaboonLoader
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.mcp.McpInputSchemaEmitter
import io.septimalmind.baboon.translator.openapi.OasTypeTranslator
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.Error2
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.files.IzFiles
import izumi.fundamentals.platform.resources.IzResources
import izumi.reflect.TagKK

import scala.jdk.CollectionConverters.*

/** T5 validity gate for the shared MCP `inputSchema` emitter
  * ([[McpInputSchemaEmitter]]).
  *
  * Two-part acceptance (decisions ledger K3/K6, goal G1):
  *
  *   (a) GOLDEN / SHAPE — representative assertions on the emitted `inputSchema`
  *       for the K6 stub model's 5 tools, covering at least the nested-DTO, ADT,
  *       enum-key-map, and recursive cases.
  *   (b) WELL-FORMEDNESS — EVERY emitted `inputSchema` is run through a REAL
  *       Draft 2020-12 JSON-Schema validator (networknt json-schema-validator)
  *       which parses the schema, resolves the local `#/$defs/...` closure, and
  *       (for representative tools) validates a conforming instance — the
  *       operational analogue of how `test-openapi` runs emitted schemas through
  *       swagger-parser.
  *
  * The fixture is the LOCKED stub model at
  * `baboon-compiler/src/test/resources/baboon/mcp-stub-ok/mcp_stub.baboon`.
  */
final class McpInputSchemaEmissionTest extends McpInputSchemaEmissionTestBase[Either]

abstract class McpInputSchemaEmissionTestBase[F[+_, +_]: Error2: TagKK: BaboonTest.BaboonTestModule] extends BaboonTest[F] {

  private val emitter = new McpInputSchemaEmitter(new OasTypeTranslator)

  private def loadStubFamily(loader: BaboonLoader[F]): F[NEList[BaboonIssue], BaboonFamily] = {
    val root = IzResources
      .getPath("baboon/mcp-stub-ok")
      .getOrElse(throw new AssertionError("mcp-stub-ok fixture not found"))
      .asInstanceOf[IzResources.LoadablePathReference]
      .path
    val baboons =
      if (root.toFile.isDirectory)
        IzFiles.walk(root.toFile).toList.filter(p => p.toFile.isFile && p.toFile.getName.endsWith(".baboon"))
      else List(root)
    loader.load(baboons)
  }

  /** The single domain of the stub family. */
  private def stubDomain(family: BaboonFamily): Domain = {
    val lineages = family.domains.toMap.values.toList
    assert(lineages.size == 1, s"expected one lineage, got ${lineages.map(_.pkg)}")
    val domains = lineages.head.versions.toMap.values.toList
    assert(domains.size == 1, s"expected one version, got ${domains.map(_.version)}")
    domains.head
  }

  /** Map of method name -> emitted inputSchema for the (single) service. */
  private def emitAllTools(domain: Domain): Map[String, Json] = {
    val services = domain.defs.meta.nodes.values.collect {
      case u: DomainMember.User =>
        u.defn match {
          case s: Typedef.Service => Some(s)
          case _                  => None
        }
    }.flatten.toList
    assert(services.size == 1, s"expected one service, got ${services.map(_.id)}")
    services.head.methods.iterator.map {
      m => m.name.name -> emitter.emitInputSchema(m.sig, domain)
    }.toMap
  }

  // ── real JSON-Schema validator (networknt, Draft 2020-12) ──────────────────

  private val factory = JsonSchemaFactory.getInstance(SpecVersion.VersionFlag.V202012)

  /** Compile a schema with the real validator. This parses the schema document
    * AND eagerly resolves every `$ref` (including the local `#/$defs/...`
    * closure); a dangling ref or malformed schema throws here. Returns the
    * compiled schema for optional instance validation.
    */
  private def compileSchema(schema: Json): com.networknt.schema.JsonSchema = {
    val cfg = SchemaValidatorsConfig.builder().build()
    factory.getSchema(schema.noSpaces, InputFormat.JSON, cfg)
  }

  private def assertWellFormed(name: String, schema: Json): org.scalatest.Assertion = {
    // `compileSchema` parses the schema document; `validate` forces eager
    // `$ref` resolution + validator-tree construction. A malformed schema or a
    // dangling `$ref` throws from one of these calls, failing the test. We do
    // NOT assert validity of the empty instance against `required` (those
    // messages are expected for non-empty request DTOs and intentionally
    // ignored). Reaching the size read below means well-formedness held.
    val messageCount =
      try compileSchema(schema).validate("{}", InputFormat.JSON).size()
      catch { case t: Throwable => fail(s"schema for tool '$name' is not well-formed JSON Schema: ${t.getMessage}\n${schema.spaces2}") }
    assert(messageCount >= 0, s"tool '$name' produced a negative validation-message count")
  }

  /** Validate a conforming instance — proves the schema (incl. its `$defs`
    * closure) actually resolves and accepts valid data, with zero validation
    * messages. */
  private def assertAccepts(name: String, schema: Json, instance: Json): org.scalatest.Assertion = {
    val compiled = compileSchema(schema)
    val messages = compiled.validate(instance.noSpaces, InputFormat.JSON).asScala.toList
    assert(messages.isEmpty, s"tool '$name' rejected a conforming instance: ${messages.map(_.getMessage)}\nschema: ${schema.spaces2}\ninstance: ${instance.noSpaces}")
  }

  /** Validate a NON-conforming instance is rejected — proves constraints are
    * live (the validator is actually enforcing the schema, not a no-op). */
  private def assertRejects(name: String, schema: Json, instance: Json): org.scalatest.Assertion = {
    val compiled = compileSchema(schema)
    val messages = compiled.validate(instance.noSpaces, InputFormat.JSON).asScala.toList
    assert(messages.nonEmpty, s"tool '$name' wrongly accepted a non-conforming instance: ${instance.noSpaces}\nschema: ${schema.spaces2}")
  }

  // ── circe field helpers ────────────────────────────────────────────────────

  private def field(j: Json, key: String): Json =
    j.asObject.flatMap(_(key)).getOrElse(fail(s"expected key '$key' in ${j.noSpaces}"))

  private def hasKey(j: Json, key: String): Boolean =
    j.asObject.exists(_.contains(key))

  private def requiredSet(j: Json): Set[String] =
    field(j, "required").asArray.toList.flatten.flatMap(_.asString).toSet

  "MCP inputSchema emitter (mcp-stub-ok fixture)" should {

    "emit a self-contained Draft 2020-12 schema for EVERY tool (well-formedness gate)" in {
      (loader: BaboonLoader[F]) =>
        for {
          family <- loadStubFamily(loader)
        } yield {
          val domain = stubDomain(family)
          val tools  = emitAllTools(domain)
          assert(
            tools.keySet == Set("listCollections", "submitComposite", "processShape", "pagePoints", "ping"),
            s"unexpected tool set: ${tools.keySet}",
          )
          // (b) EVERY emitted inputSchema must be well-formed JSON Schema.
          tools.foreach { case (name, schema) =>
            assert(schema.asObject.flatMap(_("$schema")).flatMap(_.asString).contains(McpInputSchemaEmitter.schemaDialect), s"tool '$name' missing $$schema dialect")
            assert(field(schema, "type").asString.contains("object"), s"tool '$name' root is not type:object")
            assertWellFormed(name, schema)
          }
        }
    }

    "ping: scalar-only root, no $defs (baseline standalone case)" in {
      (loader: BaboonLoader[F]) =>
        for {
          family <- loadStubFamily(loader)
        } yield {
          val schema = emitAllTools(stubDomain(family))("ping")
          assert(!hasKey(schema, "$defs"), s"ping must have no $$defs: ${schema.noSpaces}")
          assert(requiredSet(schema) == Set("seqno", "label"))
          val props = field(schema, "properties")
          assert(field(props, "seqno") == Json.obj("type" -> Json.fromString("integer"), "format" -> Json.fromString("int32")))
          assert(field(props, "label") == Json.obj("type" -> Json.fromString("string")))
          assertAccepts("ping", schema, Json.obj("seqno" -> Json.fromInt(7), "label" -> Json.fromString("hi")))
          assertRejects("ping", schema, Json.obj("seqno" -> Json.fromInt(7))) // missing required 'label'
        }
    }

    "submitComposite: nested DTO + opt[DTO] + enum + foreign-string, refs are local #/$defs" in {
      (loader: BaboonLoader[F]) =>
        for {
          family <- loadStubFamily(loader)
        } yield {
          val schema = emitAllTools(stubDomain(family))("submitComposite")
          val props  = field(schema, "properties")

          // nested: Nested -> local $defs ref (NOT #/components/schemas)
          val nestedRef = field(field(props, "nested"), "$ref").asString.getOrElse(fail("nested has no $ref"))
          assert(nestedRef.startsWith("#/$defs/"), s"nested ref must be local, got: $nestedRef")

          // maybePoint: opt[Point] -> oneOf[ {$ref}, {type:null} ]
          val maybeOneOf = field(field(props, "maybePoint"), "oneOf").asArray.getOrElse(fail("maybePoint not oneOf")).toList
          def key(j: Json, k: String): Option[Json] = j.asObject.flatMap(_(k))
          assert(maybeOneOf.exists(j => key(j, "$ref").flatMap(_.asString).exists(_.startsWith("#/$defs/"))), "maybePoint missing local $ref branch")
          assert(maybeOneOf.exists(j => key(j, "type").flatMap(_.asString).contains("null")), "maybePoint missing null branch")

          // color: Color -> local enum ref
          assert(field(field(props, "color"), "$ref").asString.exists(_.startsWith("#/$defs/")), "color ref not local")

          // fancy: FFancyStr foreign, maps to string in all 9 langs -> precise scalar, NOT opaque object
          assert(field(props, "fancy") == Json.obj("type" -> Json.fromString("string")), s"fancy foreign type not resolved to string scalar: ${field(props, "fancy").noSpaces}")

          // $defs closure contains Nested, Point, Color (FFancyStr collapsed to scalar -> not in $defs)
          val defs = field(schema, "$defs").asObject.getOrElse(fail("no $defs")).keys.toSet
          assert(defs.exists(_.endsWith("_Nested")), s"$defs missing Nested: $defs")
          assert(defs.exists(_.endsWith("_Point")), s"$defs missing Point (nested-of-nested): $defs")
          assert(defs.exists(_.endsWith("_Color")), s"$defs missing Color: $defs")
          assert(!defs.exists(_.endsWith("_FFancyStr")), s"FFancyStr must collapse to scalar, not appear in $defs: $defs")

          assertWellFormed("submitComposite", schema)
          // a conforming instance exercising nested/opt/enum/foreign-string
          val instance = Json.obj(
            "nested" -> Json.obj(
              "point" -> Json.obj("x" -> Json.fromInt(1), "y" -> Json.fromInt(2)),
              "color" -> Json.fromString("Red"),
            ),
            "maybePoint" -> Json.Null,
            "color"      -> Json.fromString("Blue"),
            "fancy"      -> Json.fromString("anything"),
          )
          assertAccepts("submitComposite", schema, instance)
          // enum constraint is live: an out-of-domain colour is rejected
          assertRejects("submitComposite", schema, instance.deepMerge(Json.obj("color" -> Json.fromString("Purple"))))
        }
    }

    "listCollections: list / set+uniqueItems / map[str] / map[enumKey] entry-objects" in {
      (loader: BaboonLoader[F]) =>
        for {
          family <- loadStubFamily(loader)
        } yield {
          val schema = emitAllTools(stubDomain(family))("listCollections")
          val props  = field(schema, "properties")

          // tags: lst[str] -> array of strings
          assert(field(props, "tags") == Json.obj("type" -> Json.fromString("array"), "items" -> Json.obj("type" -> Json.fromString("string"))))

          // uniqueIds: set[i64] -> array + uniqueItems
          val uniq = field(props, "uniqueIds")
          assert(field(uniq, "type").asString.contains("array"))
          assert(field(uniq, "uniqueItems") == Json.True)

          // labels: map[str,str] -> object + additionalProperties
          val labels = field(props, "labels")
          assert(field(labels, "type").asString.contains("object"))
          assert(field(labels, "additionalProperties") == Json.obj("type" -> Json.fromString("string")))

          // byColor: map[Color,str] (enum key) -> array of {key,value} entry objects
          val byColor = field(props, "byColor")
          assert(field(byColor, "type").asString.contains("array"))
          val entry = field(byColor, "items")
          assert(requiredSet(entry) == Set("key", "value"))
          val entryKey = field(field(entry, "properties"), "key")
          assert(field(entryKey, "$ref").asString.exists(_.startsWith("#/$defs/")), s"enum-key map entry key must be a local $$ref: ${entryKey.noSpaces}")

          assertWellFormed("listCollections", schema)
          val instance = Json.obj(
            "tags"      -> Json.arr(Json.fromString("a"), Json.fromString("b")),
            "uniqueIds" -> Json.arr(Json.fromLong(1L), Json.fromLong(2L)),
            "labels"    -> Json.obj("k" -> Json.fromString("v")),
            "byColor"   -> Json.arr(Json.obj("key" -> Json.fromString("Green"), "value" -> Json.fromString("ok"))),
          )
          assertAccepts("listCollections", schema, instance)
        }
    }

    "processShape: ADT oneOf branches + recursive Tree terminate via local $defs" in {
      (loader: BaboonLoader[F]) =>
        for {
          family <- loadStubFamily(loader)
        } yield {
          val schema = emitAllTools(stubDomain(family))("processShape")
          val props  = field(schema, "properties")
          val defs   = field(schema, "$defs").asObject.getOrElse(fail("no $defs"))

          // shape: Shape -> local ref to an ADT entry that is a oneOf of branch refs
          val shapeRef = field(field(props, "shape"), "$ref").asString.getOrElse(fail("shape no $ref"))
          assert(shapeRef.startsWith("#/$defs/"))
          val shapeDefName = shapeRef.stripPrefix("#/$defs/")
          val shapeDef     = defs(shapeDefName).getOrElse(fail(s"$shapeDefName not in $$defs"))
          val branches     = field(shapeDef, "oneOf").asArray.getOrElse(fail("Shape def not oneOf")).toList
          assert(branches.size == 2, s"Shape must have 2 branches, got ${branches.size}")
          branches.foreach(b => assert(field(b, "$ref").asString.exists(_.startsWith("#/$defs/")), "ADT branch ref not local"))
          // both branch DTOs (Circle, Rect) are present in $defs
          assert(defs.keys.exists(_.endsWith("_Circle")), s"Circle branch missing from $$defs: ${defs.keys}")
          assert(defs.keys.exists(_.endsWith("_Rect")), s"Rect branch missing from $$defs: ${defs.keys}")

          // tree: Tree -> recursive; its $defs entry self-refs locally (terminates)
          val treeRef = field(field(props, "tree"), "$ref").asString.getOrElse(fail("tree no $ref"))
          val treeDef = defs(treeRef.stripPrefix("#/$defs/")).getOrElse(fail("Tree not in $$defs"))
          val treeProps = field(treeDef, "properties")
          // children: lst[Tree] -> array whose items $ref back to Tree locally
          val childItems = field(field(treeProps, "children"), "items")
          assert(field(childItems, "$ref").asString.contains(treeRef), s"Tree.children must self-ref locally to $treeRef, got ${childItems.noSpaces}")

          assertWellFormed("processShape", schema)
          // a conforming instance with a recursive tree + an ADT branch
          val instance = Json.obj(
            "shape" -> Json.obj("radius" -> Json.fromDoubleOrNull(1.5)),
            "tree" -> Json.obj(
              "value"    -> Json.fromInt(0),
              "children" -> Json.arr(Json.obj("value" -> Json.fromInt(1), "children" -> Json.arr())),
            ),
          )
          assertAccepts("processShape", schema, instance)
        }
    }

    "pagePoints: template-instantiation alias PointPage = Page[Point] inlines as a DTO" in {
      (loader: BaboonLoader[F]) =>
        for {
          family <- loadStubFamily(loader)
        } yield {
          val schema = emitAllTools(stubDomain(family))("pagePoints")
          val props  = field(schema, "properties")
          val defs   = field(schema, "$defs").asObject.getOrElse(fail("no $defs"))

          val pageRef = field(field(props, "page"), "$ref").asString.getOrElse(fail("page no $ref"))
          assert(pageRef.startsWith("#/$defs/"))
          val pageDef = defs(pageRef.stripPrefix("#/$defs/")).getOrElse(fail("Page def missing"))
          val pageProps = field(pageDef, "properties")
          // items: lst[Point] -> array whose items $ref Point locally
          val itemsItems = field(field(pageProps, "items"), "items")
          assert(field(itemsItems, "$ref").asString.exists(_.startsWith("#/$defs/")), s"Page.items element must be a local $$ref: ${itemsItems.noSpaces}")
          // total: u32 -> integer with minimum 0
          assert(field(pageProps, "total") == Json.obj("type" -> Json.fromString("integer"), "format" -> Json.fromString("int32"), "minimum" -> Json.fromInt(0)))
          assert(defs.keys.exists(_.endsWith("_Point")), s"Point must be in $$defs: ${defs.keys}")

          assertWellFormed("pagePoints", schema)
          val instance = Json.obj(
            "page" -> Json.obj(
              "items" -> Json.arr(Json.obj("x" -> Json.fromInt(1), "y" -> Json.fromInt(2))),
              "total" -> Json.fromInt(1),
            )
          )
          assertAccepts("pagePoints", schema, instance)
        }
    }
  }
}
