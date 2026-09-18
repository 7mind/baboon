package io.septimalmind.baboon.translator.graphql

import io.septimalmind.baboon.CompilerProduct
import io.septimalmind.baboon.CompilerTarget.GqlTarget
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.{BaboonAbstractTranslator, OutputFile, Sources}
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.NEList
import io.septimalmind.baboon.parser.model.issues.TranslationIssue
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

/** Translates Baboon domain models to GraphQL Schema Definition Language (SDL).
  *
  * Emits one `schema.graphql` file per domain version. Schema-only — no codecs,
  * conversions, or runtime are generated.
  *
  * === Type mapping conventions ===
  *
  *  - '''DTO''' → `type`
  *  - '''Enum''' → `enum`
  *  - '''ADT''' → branch DTOs emitted as `type`s, then a `union` over them.
  *    Contracts and services inside ADTs are excluded from the union.
  *  - '''Foreign with `rt`''' → resolved to the underlying Baboon type
  *  - '''Foreign without `rt`''' → emitted as a custom `scalar`
  *  - '''Service / Contract''' → skipped (non-data types)
  *  - '''Type aliases''' → transparent, resolved by the typer before we see them
  *
  * === Scalar mapping ===
  *
  *  - `bit` → `Boolean`, `str` → `String`, `uid` → `ID`
  *  - `i08`/`i16`/`i32`/`u08`/`u16`/`u32` → `Int`
  *  - `f32`/`f64` → `Float`
  *  - `i64` → `BaboonInt64`, `u64` → `BaboonUInt64`, `f128` → `BaboonFloat128`
  *  - `tsu` → `BaboonDateTimeUtc`, `tso` → `BaboonDateTimeOffset`
  *  - `bytes` → `BaboonBytes`
  *
  * Custom scalars (`Baboon*`) are only emitted when actually referenced.
  *
  * === Collection mapping ===
  *
  *  - `opt[T]` → nullable field (no `!` suffix)
  *  - `lst[T]` / `set[T]` → `[T!]!` (non-null list of non-null elements)
  *  - `map[K, V]` → `[BaboonMapEntry_K_V!]!` with a helper type:
  *    {{{type BaboonMapEntry_K_V { key: K!, value: V! }}}}
  *    GraphQL has no native map type so this is the standard workaround.
  *
  * === Limitations ===
  *
  *  - No `Query` root type is emitted — the output is a type-definition library,
  *    not an executable schema.
  *  - Empty DTOs get a placeholder `_empty: Boolean` field (GraphQL forbids
  *    empty object types).
  *  - Narrowing of integer/float widths (e.g. `u08` vs `i32` both → `Int`)
  *    loses precision information at the schema level.
  *  - Field names and enum values are sanitized for GraphQL validity:
  *    names starting with digits get a `_` prefix, `__` prefix is escaped to `gql___`,
  *    and enum values `true`/`false`/`null` get a trailing `_`.
  */
class GqlBaboonTranslator[F[+_, +_]: Error2](
  target: GqlTarget,
  typeTranslator: GqlTypeTranslator,
) extends BaboonAbstractTranslator[F] {
  /** SDL carries no typed references, so the tree has no value type. */
  private type GqlTree = TextTree[Nothing]

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

    val pkg                = domain.id
    val ver                = domain.version
    val foreignResolutions = typeTranslator.foreignTypeResolution(domain)

    val members = domain.defs.meta.nodes.values.collect {
      case u: DomainMember.User => u
    }.toList.sortBy(_.id.toString)

    val resolvedFields = members.map {
      m =>
        val fields = m.defn match {
          case dto: Typedef.Dto => dto.fields
          case adt: Typedef.Adt => adt.fields
          case _                => Nil
        }
        m.id -> fields.map(f => f.copy(tpe = typeTranslator.resolveTypeRef(f.tpe, foreignResolutions)))
    }.toMap

    // Collect all map types used in fields (after resolution) so we can emit helper types
    val allMapTypes = members.flatMap {
      m =>
        m.defn match {
          case dto: Typedef.Dto =>
            resolvedFields(dto.id).flatMap(f => typeTranslator.collectMapTypes(f.tpe))
          case _ => Nil
        }
    }.toSet

    // Collect foreign types without runtime mapping — emit as custom scalars
    val foreignScalars = members.flatMap {
      m =>
        m.defn match {
          case f: Typedef.Foreign if f.runtimeMapping.isEmpty => Some(typeTranslator.typeName(f.id))
          case _                                              => None
        }
    }.sorted.distinct

    val header: GqlTree =
      q"""# Generated by Baboon compiler
         |# Domain: ${pkg.toString}, Version: ${ver.toString}""".stripMargin

    // Emit custom scalars for builtin overflows
    val builtinCustomScalars = Set(
      "BaboonInt64",
      "BaboonUInt64",
      "BaboonFloat128",
      "BaboonDateTimeUtc",
      "BaboonDateTimeOffset",
      "BaboonBytes",
      "BaboonAny",
    )

    val usedScalars   = resolvedFields.valuesIterator.flatten.flatMap(f => collectScalarsFromRef(f.tpe)).toSet
    val scalarsToEmit = (builtinCustomScalars.intersect(usedScalars).toList ++ foreignScalars).sorted

    val scalars: List[GqlTree] = scalarsToEmit.map {
      scalar =>
        val description = TextTree.verbatim[Nothing](scalarDescription(scalar).getOrElse(""))
        q"${description}scalar $scalar"
    }

    // Emit map entry types (deduplicated by generated name since different TypeRefs can map to the same GraphQL type)
    val mapEntries: List[GqlTree] = allMapTypes.toList.map {
      case (keyRef, valRef) =>
        (typeTranslator.mapEntryTypeName(keyRef, valRef), typeTranslator.fieldTypeStr(keyRef), typeTranslator.fieldTypeStr(valRef))
    }.distinctBy(_._1).sortBy(_._1).map {
      case (name, keyType, valType) =>
        q"""type $name {
           |  key: $keyType
           |  value: $valType
           |}""".stripMargin
    }

    // Emit type definitions (skip ADT-owned types — they are emitted by their parent ADT)
    val typedefs: List[GqlTree] = members.flatMap {
      m =>
        m.defn match {
          case _: Typedef.NonDataTypedef => None // skip services, contracts
          case _: Typedef.Foreign        => None // skip — handled as scalars or resolved via runtimeMapping
          case _ if m.ownedByAdt         => None // skip — emitted by parent ADT
          case defn                      => renderTypedef(defn, m, domain, resolvedFields)
        }
    }

    val scalarBlock: List[GqlTree] = if (scalars.isEmpty) Nil else List(scalars.joinN())
    val schema: GqlTree            = (List(header) ++ scalarBlock ++ mapEntries ++ typedefs).joinNN()

    val versionStr = ver.toString.replace(".", "_")
    val pkgStr     = pkg.path.toList.map(typeTranslator.sanitize).mkString("_")
    val filename   = s"$pkgStr/v$versionStr/schema.graphql"

    List(filename -> OutputFile(schema.render + "\n\n", CompilerProduct.Definition))
  }

  private def renderTypedef(
    defn: Typedef.User,
    member: DomainMember.User,
    domain: Domain,
    resolvedFields: Map[TypeId.User, List[Field]],
  ): Option[GqlTree] = {
    defn match {
      case dto: Typedef.Dto =>
        Some(renderDto(dto, member.docs, resolvedFields))

      case e: Typedef.Enum =>
        Some(renderEnum(e, member.docs))

      case adt: Typedef.Adt =>
        Some(renderAdt(adt, member.docs, domain, resolvedFields))

      case _ => None
    }
  }

  /** Doc comments are user text: interpolated verbatim so backslashes survive and `|` margins are never stripped. */
  private def description(docs: Docs, indent: String): GqlTree =
    TextTree.verbatim[Nothing](typeTranslator.renderGqlDescription(docs, indent))

  private def renderDto(dto: Typedef.Dto, docs: Docs, resolvedFields: Map[TypeId.User, List[Field]]): GqlTree = {
    val name = typeTranslator.typeName(dto.id)
    // GraphQL forbids empty object types: emit a placeholder field
    val fields: List[GqlTree] =
      if (dto.fields.isEmpty) List(q"  _empty: Boolean")
      else
        resolvedFields(dto.id).map {
          f =>
            q"${description(f.docs, "  ")}  ${typeTranslator.sanitizeName(f.name.name)}: ${typeTranslator.fieldTypeStr(f.tpe)}"
        }
    val body: GqlTree =
      q"""type $name {
         |${fields.joinN()}
         |}""".stripMargin
    q"${description(docs, "")}$body"
  }

  private def renderEnum(e: Typedef.Enum, docs: Docs): GqlTree = {
    val name   = typeTranslator.typeName(e.id)
    val values = e.members.toList.map(m => q"  ${typeTranslator.sanitizeEnumValue(m.name)}")
    val body: GqlTree =
      q"""enum $name {
         |${values.joinN()}
         |}""".stripMargin
    q"${description(docs, "")}$body"
  }

  private def renderAdt(
    adt: Typedef.Adt,
    docs: Docs,
    domain: Domain,
    resolvedFields: Map[TypeId.User, List[Field]],
  ): GqlTree = {
    import Typedef.Adt.AdtSyntax
    val name = typeTranslator.typeName(adt.id)

    // Only emit data members (skip contracts and services)
    val dataMembers = adt.dataMembers(domain)

    // Emit each branch as a type (Baboon grammar only allows DTOs as ADT branches)
    val branches: List[GqlTree] = dataMembers.toList.flatMap {
      memberId =>
        domain.defs.meta.nodes.get(memberId).toList.flatMap {
          case u: DomainMember.User =>
            u.defn match {
              case dto: Typedef.Dto => List(renderDto(dto, u.docs, resolvedFields))
              case _                => Nil // skip non-DTO members (shouldn't happen per grammar)
            }
          case _ => Nil
        }
    }
    val branchNames    = dataMembers.map(memberId => typeTranslator.typeName(memberId))
    val union: GqlTree = q"${description(docs, "")}union $name = ${branchNames.mkString(" | ")}"

    (branches :+ union).joinNN()
  }

  /** GraphQL block-string description for a custom scalar.
    *
    * Returns a complete `"""..."""` block (with trailing newline) ready to be
    * emitted before a `scalar X` declaration, or `None` if the scalar is
    * self-explanatory.
    */
  private def scalarDescription(name: String): Option[String] = {
    name match {
      case "BaboonAny" =>
        val body =
          """Opaque any-envelope. JSON serialization of a baboon AnyOpaque value.
            |
            |Wire shape: {"$ak":<int>, "$ad"?:str, "$av"?:str, "$at"?:str, "$c":<inner>}
            |  $ak — meta-kind byte (bit 0 = typeid, bit 1 = version, bit 2 = domain)
            |  $ad — domain string, present iff bit 2 set
            |  $av — version string, present iff bit 1 set
            |  $at — typeid string, present iff bit 0 set
            |  $c  — inner payload (typed JSON value or UEBA base64-bytes)
            |
            |Kind bytes (from the locked `any` qualifier table):
            |  0x07 — variant A (`any`)                          — domain + version + typeid
            |  0x03 — variant B (`any[domain:this]`)             — version + typeid
            |  0x01 — variant C (`any[domain:current]`)          — typeid only
            |  0x06 — variant D1 (`any[T]`)                      — domain + version (typeid static)
            |  0x02 — variant D2 (`any[domain:this, T]`)         — version (typeid static)
            |  0x00 — variant D3 (`any[domain:current, T]`)      — none on the wire""".stripMargin
        Some(typeTranslator.descriptionLiteral(body.split("\n", -1).toList, ""))
      case _ => None
    }
  }

  private def collectScalarsFromRef(ref: TypeRef): Set[String] = {
    ref match {
      case TypeRef.Scalar(id: TypeId.BuiltinScalar) =>
        Set(typeTranslator.scalarName(id)).filter(_.startsWith("Baboon"))
      case TypeRef.Constructor(_, args) =>
        args.toList.flatMap(collectScalarsFromRef).toSet
      case TypeRef.Any(_, underlying) =>
        Set("BaboonAny") ++ underlying.toList.flatMap(collectScalarsFromRef).toSet
      case _ =>
        Set.empty
    }
  }
}
