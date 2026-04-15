package io.septimalmind.baboon.translator.openapi

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

        flattened.toUniqueMap(c =>
          BaboonIssue.of(TranslationIssue.NonUniqueOutputFiles(c))
        )
      }
    } yield Sources(rendered)
  }

  private def translateDomain(domain: Domain): List[(String, OutputFile)] = {
    if (!target.output.products.contains(CompilerProduct.Definition)) {
      return List.empty
    }

    val pkg = domain.id
    val ver = domain.version
    val foreignResolutions = typeTranslator.foreignTypeResolution(domain)

    val members = domain.defs.meta.nodes.values.collect {
      case u: DomainMember.User => u
    }.toList.sortBy(_.id.toString)

    val schemas = members.flatMap { m =>
      m.defn match {
        case _: Typedef.NonDataTypedef                       => None
        case f: Typedef.Foreign if f.runtimeMapping.nonEmpty => None
        case f: Typedef.Foreign                              => Some(renderForeignSchema(f))
        case _ if m.ownedByAdt                               => None
        case defn                                            => Some(renderTypedef(defn, domain, foreignResolutions))
      }
    }

    val schemasJson = schemas.mkString(",\n")

    val esc = typeTranslator.escapeJson _
    val doc =
      s"""{
         |  "openapi": "3.1.0",
         |  "info": {
         |    "title": "${esc(pkg.toString)}",
         |    "version": "${esc(ver.toString)}"
         |  },
         |  "paths": {},
         |  "components": {
         |    "schemas": {
         |$schemasJson
         |    }
         |  }
         |}
         |""".stripMargin

    val versionStr = ver.toString.replace(".", "_")
    val pkgStr = pkg.path.toList.map(typeTranslator.sanitize).mkString("_")
    val filename = s"$pkgStr/v$versionStr/openapi.json"

    List(filename -> OutputFile(doc, CompilerProduct.Definition))
  }

  private def renderTypedef(
    defn: Typedef.User,
    domain: Domain,
    foreignResolutions: Map[TypeId.User, Option[TypeRef]],
  ): String = {
    defn match {
      case dto: Typedef.Dto => renderDto(dto, foreignResolutions)
      case e: Typedef.Enum  => renderEnum(e)
      case adt: Typedef.Adt => renderAdt(adt, domain, foreignResolutions)
      case other            => throw new IllegalArgumentException(s"Unexpected typedef in OpenAPI renderTypedef: ${other.id}")
    }
  }

  private def renderDto(dto: Typedef.Dto, foreignResolutions: Map[TypeId.User, Option[TypeRef]]): String = {
    val name = typeTranslator.schemaName(dto.id)
    val esc = typeTranslator.escapeJson _

    if (dto.fields.isEmpty) {
      s"""      "${esc(name)}": {"type": "object"}"""
    } else {
      val resolvedFields = dto.fields.map(f => f.copy(tpe = typeTranslator.resolveTypeRef(f.tpe, foreignResolutions)))

      val requiredFields = resolvedFields.filterNot(f => isOptional(f.tpe))
      val requiredJson =
        if (requiredFields.isEmpty) ""
        else {
          val names = requiredFields.map(f => s""""${esc(f.name.name)}"""").mkString(", ")
          s""", "required": [$names]"""
        }

      val propsJson = resolvedFields.map { f =>
        val schema = typeTranslator.typeRefSchema(f.tpe)
        s"""          "${esc(f.name.name)}": $schema"""
      }.mkString(",\n")

      s"""      "${esc(name)}": {"type": "object"$requiredJson, "properties": {
         |$propsJson
         |        }}""".stripMargin
    }
  }

  private def renderEnum(e: Typedef.Enum): String = {
    val name = typeTranslator.schemaName(e.id)
    val esc = typeTranslator.escapeJson _
    val values = e.members.toList.map(m => s""""${esc(m.name)}"""").mkString(", ")
    s"""      "${esc(name)}": {"type": "string", "enum": [$values]}"""
  }

  private def renderAdt(
    adt: Typedef.Adt,
    domain: Domain,
    foreignResolutions: Map[TypeId.User, Option[TypeRef]],
  ): String = {
    import Typedef.Adt.AdtSyntax
    val name = typeTranslator.schemaName(adt.id)
    val esc = typeTranslator.escapeJson _
    val dataMembers = adt.dataMembers(domain)

    // Emit each branch schema inline, then the union
    val branchSchemas = dataMembers.flatMap { memberId =>
      domain.defs.meta.nodes.get(memberId).collect {
        case u: DomainMember.User =>
          u.defn match {
            case dto: Typedef.Dto => renderDto(dto, foreignResolutions)
            case e: Typedef.Enum  => renderEnum(e)
            case other            => throw new IllegalArgumentException(s"Unexpected ADT branch type in OpenAPI backend: ${other.id}")
          }
      }
    }

    val branchRefs = dataMembers.map { memberId =>
      val refName = typeTranslator.schemaName(memberId)
      s"""{"$$ref": "#/components/schemas/${esc(refName)}"}"""
    }

    val refsJson = branchRefs.mkString(", ")
    val adtSchema = s"""      "${esc(name)}": {"oneOf": [$refsJson]}"""

    if (branchSchemas.nonEmpty) {
      val branchLines = branchSchemas.mkString(",\n")
      s"""$branchLines,
         |$adtSchema""".stripMargin
    } else {
      adtSchema
    }
  }

  private def renderForeignSchema(f: Typedef.Foreign): String = {
    val name = typeTranslator.schemaName(f.id)
    val esc = typeTranslator.escapeJson _
    s"""      "${esc(name)}": {"type": "object", "description": "Foreign type: ${esc(name)}"}"""
  }

  private def isOptional(ref: TypeRef): Boolean = {
    ref match {
      case TypeRef.Constructor(TypeId.Builtins.opt, _) => true
      case _                                           => false
    }
  }

}
