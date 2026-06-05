package io.septimalmind.baboon.translator.mcp

import io.circe.Json
import io.septimalmind.baboon.translator.openapi.OasTypeTranslator
import io.septimalmind.baboon.typer.model.*

import scala.collection.mutable

/** Shared, language-agnostic MCP `inputSchema` emitter (decisions ledger K3).
  *
  * Every per-language MCP generator calls this single component to turn a
  * service method's request DTO into the tool's `inputSchema`: a SINGLE,
  * self-contained JSON Schema (Draft 2020-12) object that validates a
  * conforming argument object with NO external `$ref` resolution context.
  *
  * Contrast with the OpenAPI emitter (`OasBaboonTranslator`), which is built for
  * the OpenAPI document model — one document, a shared `components/schemas`
  * registry, and intra-document `#/components/schemas/<Name>` refs. That
  * assembly model is incompatible with MCP's standalone-per-tool contract (see
  * `docs/research/mcp-inputschema-gap-analysis.md`). This emitter REUSES the
  * scalar/`any`-envelope fragment logic (via [[OasTypeTranslator]]) but performs
  * its OWN assembly:
  *
  *   1. The request DTO becomes the schema ROOT (its `properties`/`required`
  *      inlined at the top level — there is no per-tool wrapper object).
  *   2. The reachable named-type closure (nested DTOs, enums, ADTs and their
  *      branches, collection element types, recursive self-refs, alias targets)
  *      is bundled into a LOCAL `$defs` block.
  *   3. Every reference to a named type is rewritten to `#/$defs/<Name>` so the
  *      schema resolves entirely within itself — including recursive types,
  *      which terminate against their own `$defs` entry.
  *
  * Foreign types: per-language string mappings are resolved to a precise scalar
  * (`{"type":"string"}`) when EVERY language target maps the type to one of that
  * language's string types (the determinable case from the T4 gap analysis,
  * row 9); otherwise the type is kept as a documented opaque object, as the
  * OpenAPI emitter does. A foreign type carrying an explicit Baboon `rt` mapping
  * is resolved to the underlying Baboon type first.
  *
  * The emitter is pure / deterministic / stateless and depends only on the typed
  * model, so it lives in the shared `baboon` sources and compiles under
  * Scala.js.
  */
class McpInputSchemaEmitter(typeTranslator: OasTypeTranslator) {

  import McpInputSchemaEmitter.*

  /** Emit the standalone `inputSchema` for one service method's request DTO.
    *
    * @param requestSig the method's `sig` — a `TypeRef` to the reified `_in`
    *                   request DTO (`MethodDef.sig`).
    * @param domain     the typed domain the method belongs to.
    * @return a self-contained JSON Schema object (the MCP tool `inputSchema`).
    */
  def emitInputSchema(requestSig: TypeRef, domain: Domain): Json = {
    val ctx = ForeignContext(
      resolutions = typeTranslator.foreignTypeResolution(domain),
      defs        = foreignDefsOf(domain),
    )

    val rootId = requestSig match {
      case TypeRef.Scalar(id: TypeId.User) => id
      case other =>
        throw new IllegalArgumentException(
          s"MCP request signature must reference a reified request DTO, got: $other"
        )
    }

    val rootDefn = userDefn(domain, rootId).getOrElse(
      throw new IllegalArgumentException(s"MCP request DTO not found in domain: $rootId")
    )

    val rootDto = rootDefn.defn match {
      case dto: Typedef.Dto => dto
      case other            => throw new IllegalArgumentException(s"MCP request type must be a DTO, got: ${other.getClass.getSimpleName} for $rootId")
    }

    // Collect the reachable named-type closure, then emit each into $defs.
    val closure = reachableClosure(domain, rootDto, ctx)
    val defs    = mutable.LinkedHashMap.empty[String, Json]
    closure.toList.sortBy(_.toString).foreach {
      id =>
        userDefn(domain, id).foreach {
          m =>
            emitNamedType(m, domain, ctx).foreach {
              case (name, schema) => defs.put(name, schema)
            }
        }
    }

    val base = Json.obj(
      schemaKeyword -> Json.fromString(schemaDialect),
      typeKeyword   -> Json.fromString("object"),
    )

    val withRoot = mergeObjects(base, dtoBody(rootDto, ctx))

    if (defs.isEmpty) withRoot
    else mergeObjects(withRoot, Json.obj(defsKeyword -> Json.obj(defs.toList*)))
  }

  // ── closure computation ──────────────────────────────────────────────────

  /** All named user types transitively reachable from the root DTO that must
    * appear in `$defs`. Excludes the root itself (inlined as the schema root)
    * and excludes foreign types resolvable to a precise scalar (inlined at the
    * reference site, not bundled).
    */
  private def reachableClosure(domain: Domain, rootDto: Typedef.Dto, ctx: ForeignContext): Set[TypeId.User] = {
    val seen    = mutable.LinkedHashSet.empty[TypeId.User]
    val pending = mutable.Queue.empty[TypeId.User]

    def enqueueRefs(ref: TypeRef): Unit = referencedUserTypes(ref, ctx).foreach {
      id => if (!seen.contains(id)) pending.enqueue(id)
    }

    // Seed from the root DTO's fields (the root itself is NOT in $defs).
    rootDto.fields.foreach(f => enqueueRefs(f.tpe))

    while (pending.nonEmpty) {
      val id = pending.dequeue()
      if (!seen.contains(id)) {
        seen.add(id)
        userDefn(domain, id).foreach {
          m =>
            m.defn match {
              case dto: Typedef.Dto =>
                dto.fields.foreach(f => enqueueRefs(f.tpe))
              case adt: Typedef.Adt =>
                // ADT branches are named types reachable from the union.
                adtDataMembers(adt, domain).foreach {
                  bid => if (!seen.contains(bid)) pending.enqueue(bid)
                }
              case _ => ()
            }
        }
      }
    }
    seen.toSet
  }

  /** The named user types directly referenced by a single type ref, descending
    * through collection constructors. Foreign types that resolve to a precise
    * inline scalar are excluded (not bundled into `$defs`); foreign types with a
    * Baboon `rt` mapping are resolved through before inspection.
    */
  private def referencedUserTypes(ref: TypeRef, ctx: ForeignContext): List[TypeId.User] = {
    typeTranslator.resolveTypeRef(ref, ctx.resolutions) match {
      case TypeRef.Scalar(id: TypeId.User) =>
        foreignScalar(ctx.defs.get(id)) match {
          case Some(_) => List.empty // inlined as a scalar, not a $defs entry
          case None    => List(id)
        }
      case TypeRef.Scalar(_)            => List.empty
      case TypeRef.Constructor(_, args) => args.toList.flatMap(referencedUserTypes(_, ctx))
      case _: TypeRef.Any               => List.empty
    }
  }

  // ── per-named-type emission ──────────────────────────────────────────────

  /** Emit a named type as zero or more `(defsKey, schema)` pairs. A DTO/enum is
    * one entry; an ADT contributes its own `oneOf` entry (its branches are
    * separate closure members emitted independently).
    */
  private def emitNamedType(m: DomainMember.User, domain: Domain, ctx: ForeignContext): List[(String, Json)] = {
    val name = typeTranslator.schemaName(m.id)
    m.defn match {
      case _: Typedef.NonDataTypedef =>
        // Defense-in-depth (D2): a contract/service is not a data type and must
        // never reach the closure — the upstream validator guard
        // (`BaboonValidator.checkDataTypeFields`, VerificationIssue.DataTypeExpectedField)
        // rejects a plain field whose resolved user type is a NonDataTypedef before
        // codegen. If one still arrives here, emitting `List.empty` while the
        // reference site already produced a `#/$defs/<Name>` ref would yield a
        // dangling ref; fail eagerly instead (matching the eager-failure style at
        // the `other` arm below and in `fieldSchema`).
        throw new IllegalArgumentException(
          s"Contract/service '${m.id}' reached the MCP inputSchema closure as a data type — this must be rejected upstream by the validator (D2 guard)."
        )
      case _: Typedef.Foreign =>
        // A foreign type that survived into the closure had no precise scalar
        // resolution; emit the documented opaque object.
        List(name -> foreignOpaqueSchema(name))
      case dto: Typedef.Dto =>
        List(name -> mergeObjects(Json.obj(typeKeyword -> Json.fromString("object")), dtoBody(dto, ctx)))
      case e: Typedef.Enum =>
        List(name -> enumSchema(e))
      case adt: Typedef.Adt =>
        List(name -> adtSchema(adt, domain))
      case other =>
        throw new IllegalArgumentException(s"Unexpected typedef in MCP inputSchema emitter: ${other.id}")
    }
  }

  /** The `properties` + `required` body of a DTO (without the enclosing
    * `{"type":"object"}`; callers prepend it so the same body serves both the
    * inlined root and a `$defs` entry).
    */
  private def dtoBody(dto: Typedef.Dto, ctx: ForeignContext): Json = {
    if (dto.fields.isEmpty) {
      Json.obj()
    } else {
      val required  = dto.fields.filterNot(f => isOptional(f.tpe)).map(f => Json.fromString(f.name.name))
      val props     = dto.fields.map(f => f.name.name -> fieldSchema(f.tpe, ctx))
      val withProps = Json.obj(propertiesKeyword -> Json.obj(props*))
      if (required.isEmpty) withProps
      else mergeObjects(withProps, Json.obj(requiredKeyword -> Json.arr(required*)))
    }
  }

  private def enumSchema(e: Typedef.Enum): Json = {
    val values = e.members.toList.map(m => Json.fromString(m.name))
    Json.obj(
      typeKeyword -> Json.fromString("string"),
      enumKeyword -> Json.arr(values*),
    )
  }

  private def adtSchema(adt: Typedef.Adt, domain: Domain): Json = {
    val branchRefs = adtDataMembers(adt, domain).map(localRef)
    Json.obj(oneOfKeyword -> Json.arr(branchRefs*))
  }

  private def foreignOpaqueSchema(name: String): Json =
    Json.obj(
      typeKeyword        -> Json.fromString("object"),
      descriptionKeyword -> Json.fromString(s"Foreign type: $name (no precise scalar mapping; opaque)"),
    )

  // ── field-level fragment generation (reuses OAS scalar/any logic) ──────────

  /** JSON Schema for a single field's type, with named-type refs rewritten to
    * local `#/$defs/<Name>`. Scalars and the `any` envelope reuse the OAS
    * fragment generator verbatim; collections recurse so element refs are local.
    */
  private def fieldSchema(ref: TypeRef, ctx: ForeignContext): Json = {
    typeTranslator.resolveTypeRef(ref, ctx.resolutions) match {
      case TypeRef.Scalar(id: TypeId.BuiltinScalar) =>
        parseFragment(typeTranslator.scalarSchemaJson(id))

      case TypeRef.Scalar(id: TypeId.User) =>
        foreignScalar(ctx.defs.get(id)) match {
          case Some(scalar) => parseFragment(typeTranslator.scalarSchemaJson(scalar))
          case None         => localRef(id)
        }

      case TypeRef.Constructor(TypeId.Builtins.opt, args) =>
        Json.obj(oneOfKeyword -> Json.arr(fieldSchema(args.head, ctx), Json.obj(typeKeyword -> Json.fromString("null"))))

      case TypeRef.Constructor(TypeId.Builtins.lst, args) =>
        Json.obj(typeKeyword -> Json.fromString("array"), itemsKeyword -> fieldSchema(args.head, ctx))

      case TypeRef.Constructor(TypeId.Builtins.set, args) =>
        Json.obj(typeKeyword -> Json.fromString("array"), itemsKeyword -> fieldSchema(args.head, ctx), uniqueItemsKeyword -> Json.True)

      case TypeRef.Constructor(TypeId.Builtins.map, args) =>
        mapSchema(args.head, args.tail.head, ctx)

      case _: TypeRef.Any =>
        parseFragment(typeTranslator.baboonAnySchema)

      case other =>
        throw new IllegalArgumentException(s"Unexpected type reference in MCP inputSchema emitter: ${other.id.name.name}")
    }
  }

  /** Map schema: string-keyed → object+additionalProperties; non-string-keyed
    * (enum/foreign/id key) → array of `{key,value}` entry objects (JSON has no
    * native non-string-keyed map). Mirrors the OAS map shape but with local
    * element refs.
    */
  private def mapSchema(keyRef: TypeRef, valRef: TypeRef, ctx: ForeignContext): Json = {
    val valSchema = fieldSchema(valRef, ctx)
    if (isStringKey(keyRef)) {
      Json.obj(typeKeyword -> Json.fromString("object"), additionalPropertiesKeyword -> valSchema)
    } else {
      val keySchema = fieldSchema(keyRef, ctx)
      val entry = Json.obj(
        typeKeyword       -> Json.fromString("object"),
        requiredKeyword   -> Json.arr(Json.fromString("key"), Json.fromString("value")),
        propertiesKeyword -> Json.obj("key" -> keySchema, "value" -> valSchema),
      )
      Json.obj(typeKeyword -> Json.fromString("array"), itemsKeyword -> entry)
    }
  }

  private def isStringKey(ref: TypeRef): Boolean = ref match {
    case TypeRef.Scalar(TypeId.Builtins.str) => true
    case TypeRef.Scalar(TypeId.Builtins.uid) => true
    case _                                   => false
  }

  // ── foreign-type scalar resolution ────────────────────────────────────────

  /** Precise scalar for a foreign type when every per-language binding maps to a
    * string type of its language (T4 gap row 9). Returns `None` (keep opaque)
    * if any binding is absent or maps to a non-string declaration, or if the
    * foreign type carries a Baboon `rt` mapping (resolved earlier).
    */
  private def foreignScalar(foreign: Option[Typedef.Foreign]): Option[TypeId.BuiltinScalar] =
    foreign.flatMap {
      f =>
        if (f.runtimeMapping.nonEmpty) None
        else {
          val decls = f.bindings.values.collect {
            case Typedef.ForeignEntry(_, Typedef.ForeignMapping.Custom(decl, _)) => decl
          }.toList
          val allString = decls.nonEmpty && decls.forall(d => stringTypeNames.contains(d))
          if (allString) Some(TypeId.Builtins.str) else None
        }
    }

  // ── helpers ───────────────────────────────────────────────────────────────

  private def localRef(id: TypeId.User): Json =
    Json.obj(refKeyword -> Json.fromString(s"#/$defsSegment/${typeTranslator.schemaName(id)}"))

  private def userDefn(domain: Domain, id: TypeId.User): Option[DomainMember.User] =
    domain.defs.meta.nodes.get(id).collect { case u: DomainMember.User => u }

  private def adtDataMembers(adt: Typedef.Adt, domain: Domain): List[TypeId.User] = {
    import Typedef.Adt.AdtSyntax
    adt.dataMembers(domain).toList
  }

  private def isOptional(ref: TypeRef): Boolean = ref match {
    case TypeRef.Constructor(TypeId.Builtins.opt, _) => true
    case _                                           => false
  }

  private def foreignDefsOf(domain: Domain): Map[TypeId.User, Typedef.Foreign] =
    domain.defs.meta.nodes.values.collect {
      case u: DomainMember.User =>
        u.defn match {
          case f: Typedef.Foreign => Some(f.id -> f)
          case _                  => None
        }
    }.flatten.toMap

  /** Parse a JSON fragment string produced by the OAS fragment generator into a
    * `Json` value. The OAS generator returns well-formed JSON object literals;
    * a parse failure indicates an emitter defect and is surfaced eagerly.
    */
  private def parseFragment(fragment: String): Json =
    io.circe.parser.parse(fragment) match {
      case Right(j)  => j
      case Left(err) => throw new IllegalStateException(s"OAS fragment did not parse as JSON: $fragment ($err)")
    }

  /** Shallow object merge: `b`'s keys extend / override `a`'s. Both must be JSON
    * objects (the emitter only ever merges object schemas).
    */
  private def mergeObjects(a: Json, b: Json): Json =
    (a.asObject, b.asObject) match {
      case (Some(oa), Some(ob)) => Json.fromJsonObject(ob.toIterable.foldLeft(oa) { case (acc, (k, v)) => acc.add(k, v) })
      case _                    => throw new IllegalStateException(s"mergeObjects expects two objects, got: $a / $b")
    }
}

object McpInputSchemaEmitter {
  final val schemaDialect = "https://json-schema.org/draft/2020-12/schema"

  final val schemaKeyword               = "$schema"
  final val typeKeyword                 = "type"
  final val propertiesKeyword           = "properties"
  final val requiredKeyword             = "required"
  final val enumKeyword                 = "enum"
  final val oneOfKeyword                = "oneOf"
  final val itemsKeyword                = "items"
  final val uniqueItemsKeyword          = "uniqueItems"
  final val additionalPropertiesKeyword = "additionalProperties"
  final val descriptionKeyword          = "description"
  final val refKeyword                  = "$ref"
  final val defsKeyword                 = "$defs"
  final val defsSegment                 = "$defs"

  /** Foreign-type lookup context threaded through one emission: the OAS
    * Baboon→Baboon resolution map plus the foreign typedefs (for per-language
    * binding inspection). Kept local to a call so the emitter stays stateless.
    */
  private final case class ForeignContext(
    resolutions: Map[TypeId.User, Option[TypeRef]],
    defs: Map[TypeId.User, Typedef.Foreign],
  )

  /** Per-language declaration strings that denote that language's native string
    * type. A foreign type all of whose bindings are in this set collapses to a
    * precise `{"type":"string"}` MCP schema instead of an opaque object.
    */
  final val stringTypeNames: Set[String] = Set(
    "System.String", // cs
    "java.lang.String", // scala / kotlin / java
    "kotlin.String", // kotlin (alt)
    "builtins.str", // py
    "str", // py (alt)
    "std::string::String", // rust
    "String", // rust / swift (alt)
    "string", // typescript
    "dart.core.String", // dart
    "Swift.String", // swift
  )
}
