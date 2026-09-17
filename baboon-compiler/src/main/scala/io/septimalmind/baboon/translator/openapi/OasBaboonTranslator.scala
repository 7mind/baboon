package io.septimalmind.baboon.translator.openapi

import io.circe.Json
import io.septimalmind.baboon.translator.schema.SchemaReferences
import io.septimalmind.baboon.CompilerProduct
import io.septimalmind.baboon.CompilerTarget.OasTarget
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.{BaboonAbstractTranslator, OutputFile, Sources}
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.NEList
import io.septimalmind.baboon.parser.model.issues.TranslationIssue

/** Translates Baboon domain models to OpenAPI 3.1 component schemas.
  *
  * Emits one `openapi.json` file per domain version containing an OpenAPI 3.1
  * document with all type definitions under `components/schemas`. Schema-only --
  * no paths, codecs, conversions, or runtime are generated.
  *
  * === Type mapping conventions ===
  *
  *  - '''DTO''' -> JSON Schema `object` with `properties` and `required`
  *  - '''Enum''' -> JSON Schema `string` with `enum` keyword
  *  - '''ADT''' -> `oneOf` referencing each branch schema
  *  - '''Foreign with `rt`''' -> resolved to the underlying Baboon type
  *  - '''Foreign without `rt`''' -> opaque `object` with a description
  *  - '''Service / Contract''' -> skipped (non-data types)
  *  - '''Type aliases''' -> transparent, resolved by the typer before we see them
  *
  * === Scalar mapping (Baboon -> JSON Schema type/format) ===
  *
  *  - `bit` -> `boolean`
  *  - `str` -> `string`
  *  - `i08`/`i16`/`i32` -> `integer` / `int32`
  *  - `i64` -> `integer` / `int64`
  *  - `u08`/`u16`/`u32` -> `integer` / `int32` with `minimum: 0`
  *  - `u64` -> `integer` / `int64` with `minimum: 0`
  *  - `f32` -> `number` / `float`
  *  - `f64` -> `number` / `double`
  *  - `f128` -> `string` / `decimal` (no native 128-bit float in JSON)
  *  - `uid` -> `string` / `uuid`
  *  - `tsu` / `tso` -> `string` / `date-time`
  *  - `bytes` -> `string` / `byte` (base64-encoded)
  *
  * === Collection mapping ===
  *
  *  - `opt[T]` -> `oneOf: [T, {type: "null"}]` (JSON Schema 2020-12 nullable)
  *  - `lst[T]` -> `{type: "array", items: T}`
  *  - `set[T]` -> `{type: "array", items: T, uniqueItems: true}`
  *  - `map[str, V]` -> `{type: "object", additionalProperties: V}`
  *  - `map[K, V]` (non-string key) -> `{type: "array", items: {type: "object",
  *    properties: {key: K, value: V}}}` -- JSON has no native map-with-non-string-keys,
  *    so we fall back to an array of key-value entry objects.
  *
  * === Limitations ===
  *
  *  - `paths` is always empty -- the output is a component-schema library, not a
  *    full API specification. Consumers should `$ref` into `components/schemas`.
  *  - No discriminator `mapping` is emitted for ADTs because branch schemas are
  *    always in the same document; consumers can rely on `oneOf` + schema structure.
  *  - Unsigned integer constraints (`minimum: 0`) are advisory -- JSON Schema
  *    validators enforce them, but some OpenAPI tooling may ignore `minimum` on
  *    integer schemas.
  *  - `f128` is represented as a string since JSON numbers cannot carry 128-bit
  *    precision; consumers must parse the string value themselves.
  *  - `tsu` and `tso` both map to `date-time`; the distinction between UTC and
  *    offset timestamps is lost at the schema level.
  *  - Empty DTOs produce `{type: "object"}` with no properties.
  */
class OasBaboonTranslator[F[+_, +_]: Error2](
  target: OasTarget,
  typeTranslator: OasTypeTranslator,
) extends BaboonAbstractTranslator[F] {

  override def translate(family: BaboonFamily): F[NEList[BaboonIssue], Sources] = {
    for {
      rendered <- F.fromEither {
        val allFiles = family.domains.iterator.flatMap {
          case (_, lineage) =>
            lineage.versions.iterator.map {
              case (_, domain) => translateDomain(domain)
            }
        }.toList

        val flattened = allFiles.flatMap(identity)

        flattened.toUniqueMap(c => BaboonIssue.of(TranslationIssue.NonUniqueOutputFiles(c)))
      }
    } yield Sources(rendered)
  }

  private def translateDomain(domain: Domain): List[(String, OutputFile)] = {
    if (!target.output.products.contains(CompilerProduct.Definition)) {
      return List.empty
    }

    val references = SchemaReferences.prepare(domain)
    val members = domain.defs.meta.nodes.values.collect {
      case u: DomainMember.User => u
    }.toList.sortBy(_.id.toString)

    val schemas = members.flatMap {
      m =>
        m.defn match {
          case _: Typedef.NonDataTypedef                       => Nil
          case f: Typedef.Foreign if f.runtimeMapping.nonEmpty => Nil
          case f: Typedef.Foreign =>
            List(
              typeTranslator.schemaName(f.id) -> Json.obj(
                "type"        -> Json.fromString("object"),
                "description" -> Json.fromString(s"Foreign type: ${typeTranslator.schemaName(f.id)}"),
              )
            )
          case _ if m.ownedByAdt => Nil
          case defn              => renderTypedef(defn, m.docs, domain, references)
        }
    }

    val doc = Json.obj(
      "openapi"    -> Json.fromString("3.1.0"),
      "info"       -> Json.obj("title" -> Json.fromString(domain.id.toString), "version" -> Json.fromString(domain.version.toString)),
      "paths"      -> Json.obj(),
      "components" -> Json.obj("schemas" -> Json.obj(schemas*)),
    )
    val versionStr = domain.version.toString.replace(".", "_")
    val pkgStr     = domain.id.path.toList.map(typeTranslator.sanitize).mkString("_")
    List(s"$pkgStr/v$versionStr/openapi.json" -> OutputFile(doc.spaces2 + "\n", CompilerProduct.Definition))
  }

  private def renderTypedef(defn: Typedef.User, docs: Docs, domain: Domain, references: SchemaReferences): List[(String, Json)] = {
    defn match {
      case dto: Typedef.Dto => List(typeTranslator.schemaName(dto.id) -> renderDto(dto, docs, references))
      case e: Typedef.Enum  => List(typeTranslator.schemaName(e.id) -> renderEnum(e, docs))
      case adt: Typedef.Adt => renderAdt(adt, docs, domain, references)
      case other            => throw new IllegalArgumentException(s"Unexpected typedef in OpenAPI renderTypedef: ${other.id}")
    }
  }

  private def described(schema: Json, docs: Docs): Json = {
    typeTranslator.renderOasDescription(docs) match {
      // Preserve the built-in envelope description's precedence over field docs.
      case Some(description) if !schema.hcursor.downField("description").succeeded =>
        schema.mapObject(_.add("description", Json.fromString(description)))
      case _ => schema
    }
  }

  private def renderDto(dto: Typedef.Dto, docs: Docs, references: SchemaReferences): Json = {
    val base = described(Json.obj("type" -> Json.fromString("object")), docs)
    if (dto.fields.isEmpty) base
    else {
      val fields   = dto.fields.map(f => f.copy(tpe = references.resolve(f.tpe)))
      val required = fields.filterNot(f => isOptional(f.tpe)).map(f => Json.fromString(f.name.name))
      val properties = fields.map {
        f =>
          f.name.name -> described(typeTranslator.typeRefSchemaValue(f.tpe, references.enums.keySet), f.docs)
      }
      val withRequired = if (required.isEmpty) base else base.mapObject(_.add("required", Json.arr(required*)))
      withRequired.mapObject(_.add("properties", Json.obj(properties*)))
    }
  }

  private def renderEnum(e: Typedef.Enum, docs: Docs): Json =
    described(Json.obj("type" -> Json.fromString("string")), docs)
      .mapObject(_.add("enum", Json.arr(e.members.toList.map(m => Json.fromString(m.name))*)))

  private def renderAdt(adt: Typedef.Adt, docs: Docs, domain: Domain, references: SchemaReferences): List[(String, Json)] = {
    import Typedef.Adt.AdtSyntax
    val dataMembers = adt.dataMembers(domain).toList
    val wrapperNote = typeTranslator.adtWrapperDoc(dataMembers.map(_.name.name))
    val description = typeTranslator.renderOasDescription(docs).map(d => s"$d\n\n$wrapperNote").getOrElse(wrapperNote)
    val branches = dataMembers.flatMap {
      memberId =>
        domain.defs.meta.nodes.get(memberId).collect {
          case u: DomainMember.User =>
            val schema = u.defn match {
              case dto: Typedef.Dto => renderDto(dto, u.docs, references)
              case e: Typedef.Enum  => renderEnum(e, u.docs)
              case other            => throw new IllegalArgumentException(s"Unexpected ADT branch type in OpenAPI backend: ${other.id}")
            }
            typeTranslator.schemaName(memberId) -> schema
        }
    }
    branches :+ (typeTranslator.schemaName(adt.id) -> Json.obj(
      "oneOf"       -> Json.arr(dataMembers.map(typeTranslator.componentRef)*),
      "description" -> Json.fromString(description),
    ))
  }

  private def isOptional(ref: TypeRef): Boolean = ref match {
    case TypeRef.Constructor(TypeId.Builtins.opt, _) => true
    case _                                           => false
  }
}
