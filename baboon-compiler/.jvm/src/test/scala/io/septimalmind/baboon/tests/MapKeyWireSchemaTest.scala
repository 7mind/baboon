package io.septimalmind.baboon.tests

import com.networknt.schema.{InputFormat, SchemaRegistry, SpecificationVersion}
import io.circe.Json
import io.circe.parser.parse
import io.septimalmind.baboon.BaboonLoader
import io.septimalmind.baboon.translator.mcp.McpInputSchemaEmitter
import io.septimalmind.baboon.translator.openapi.OasTypeTranslator
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.files.IzFiles
import izumi.fundamentals.platform.resources.IzResources

import scala.jdk.CollectionConverters.*

/** Every JSON codec writes `map[K, V]` as a string-keyed object whatever `K` is (docs/json-codecs.md,
  * "Map keys"), so the OpenAPI and MCP schemas must describe that object, not an array of
  * `{key, value}` entries (https://github.com/7mind/baboon/issues/95).
  */
final class MapKeyWireSchemaTest extends BaboonTest[Either] {
  /** What the generated TypeScript codec writes for one entry per map (taken from the issue). */
  private val CodecDocument = parse(
    """{
      |  "byStr": {"a": 1},
      |  "byUid": {"11111111-2222-3333-4444-555555555555": 1},
      |  "byEnum": {"Red": 1},
      |  "byInt": {"7": 1},
      |  "byStrId": {"StrId:1.0.0#value:a": 1},
      |  "byUidId": {"UidId:1.0.0#value:11111111-2222-3333-4444-555555555555": 1},
      |  "byWrapper": {"a": 1}
      |}""".stripMargin
  ).fold(e => throw e, identity)

  private val registry = SchemaRegistry.withDefaultDialect(SpecificationVersion.DRAFT_2020_12)

  private def violations(schema: Json, instance: Json): List[String] = {
    registry.getSchema(schema.noSpaces, InputFormat.JSON).validate(instance.noSpaces, InputFormat.JSON).asScala.toList.map(_.getMessage)
  }

  private def domain(loader: BaboonLoader[Either]): Domain = {
    val root   = IzResources.getPath("map-keys-ok").get.asInstanceOf[IzResources.LoadablePathReference].path
    val files  = IzFiles.walk(root.toFile).toList.filter(p => p.toFile.isFile && p.toFile.getName.endsWith(".baboon"))
    val family = loader.load(files).fold(issues => fail(s"fixture failed to load: $issues"), identity)
    family.domains.toMap.values.head.versions.toMap.values.head
  }

  private def user(domain: Domain, name: String): Typedef.User = {
    domain.defs.meta.nodes.collectFirst {
      case (id: TypeId.User, member: DomainMember.User) if id.name.name == name && id.owner == Owner.Toplevel => member.defn
    }.getOrElse(fail(s"$name not found"))
  }

  "map schemas" should {
    "describe every OpenAPI map field as the string-keyed object the JSON codecs write" in {
      (loader: BaboonLoader[Either]) =>
        val dom        = domain(loader)
        val translator = new OasTypeTranslator
        val enumKeys   = translator.enumKeysOf(dom)
        val holder = user(dom, "Holder") match {
          case dto: Typedef.Dto => dto
          case other            => fail(s"Holder is $other")
        }
        holder.fields.foreach {
          field =>
            val schema = parse(translator.typeRefSchema(field.tpe, enumKeys)).fold(e => fail(e.toString), identity)
            assert(schema.hcursor.get[String]("type") == Right("object"), s"${field.name.name}: ${schema.noSpaces}")
            assert(schema.hcursor.downField("additionalProperties").succeeded, s"${field.name.name}: ${schema.noSpaces}")
            // the enum key's propertyNames is a component $ref, resolvable only inside the whole document
            if (field.name.name != "byEnum") {
              val value = CodecDocument.hcursor.downField(field.name.name).focus.get
              assert(violations(schema, value) == Nil, s"${field.name.name}: ${schema.noSpaces}")
            }
        }
    }

    "accept the codecs' JSON in the MCP inputSchema" in {
      (loader: BaboonLoader[Either]) =>
        val dom = domain(loader)
        val service = user(dom, "MapTools") match {
          case s: Typedef.Service => s
          case other              => fail(s"MapTools is $other")
        }
        val emitter = new McpInputSchemaEmitter(new OasTypeTranslator)
        val schema  = emitter.emitInputSchema(service.methods.head.sig, emitter.prepare(dom))
        assert(violations(schema, CodecDocument) == Nil, schema.spaces2)
        // still constrained: an entry array is not the wire form
        assert(violations(schema, CodecDocument.mapObject(_.add("byInt", parse("""[{"key": 7, "value": 1}]""").toOption.get))).nonEmpty)
        assert(violations(schema, CodecDocument.mapObject(_.add("byEnum", parse("""{"Blue": 1}""").toOption.get))).nonEmpty)
    }
  }
}
