package io.septimalmind.baboon.parser.defns

import fastparse.*
import io.septimalmind.baboon.parser.{ParserContext, model}
import io.septimalmind.baboon.parser.defns.base.{DefDocs, idt, kw, struct}
import io.septimalmind.baboon.parser.model.RawDtoMember.ContractRef
import io.septimalmind.baboon.parser.model.{ExtractionKind, RawContractRef, RawDocComment, RawDocs, RawDto, RawDtoMember, RawField, RawFieldName, RawIdentifier, RawTypeName, RawTypeRef, ScopedRef}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.language.Quirks.Discarder

class DefDto(context: ParserContext, meta: DefMeta, docs: DefDocs) {
  context.discard()

  def typeParams[$: P]: P[NEList[RawTypeRef]] = {
    import fastparse.SingleLineWhitespace.whitespace
    ("[" ~ typeRef.rep(min = 1, sep = ",") ~ "]")
      .map(p => NEList.unsafeFrom(p.toList))
  }

  /** Parses the optional type-parameter head `[T1, T2, …]` on a template declaration.
    *
    * Grammar: `"[" idt.symbol ("," idt.symbol)* "]"` — bare identifiers only (NOT typeRef), at
    * least one parameter required so that `[]` produces a parse failure.
    */
  def templateHead[$: P]: P[List[RawTypeName]] = {
    import fastparse.SingleLineWhitespace.whitespace
    ("[" ~ idt.symbol.rep(min = 1, sep = ",") ~ "]")
      .map(p => p.toList.map(RawTypeName.apply))
  }

  def nonGenericTypeRef[$: P]: P[ScopedRef] = {
    idt.symbolSeq.map(s => ScopedRef(NEList.unsafeFrom(s.map(p => RawTypeName(p)).toList)))
  }

  private def anyQualifier[$: P]: P[RawTypeRef.AnyRef.Qual] = {
    // Qualifier tokens have no internal whitespace (e.g. `domain: this` must not parse).
    // Pin the no-whitespace contract at compile time: if a future edit introduces a `~` inside
    // this body it will use `NoWhitespace`, preserving the strict behavior instead of silently
    // inheriting the caller's `SingleLineWhitespace`. `.discard()` keeps the implicit in scope
    // without triggering the "unused import" warning when no `~` is currently present.
    import fastparse.NoWhitespace.noWhitespaceImplicit
    noWhitespaceImplicit.discard()
    P("domain:this").map(_ => RawTypeRef.AnyRef.DomainThis) |
    P("domain:current").map(_ => RawTypeRef.AnyRef.DomainCurrent)
  }

  private def anyTypeRefArgs[$: P]: P[(Option[RawTypeRef.AnyRef.Qual], Option[RawTypeRef])] = {
    import fastparse.SingleLineWhitespace.whitespace
    // Strict arg order: qualifier first (optional), then underlying typeRef (optional). Empty brackets rejected.
    //
    // Leading `CharsWhileIn(" \t", 0)` explicitly consumes any spaces/tabs between the `any`
    // identifier and `[`. We cannot rely on the surrounding implicit: this parser is invoked
    // from a `flatMap` continuation in `typeRef`, which does not insert a `~` boundary before
    // the call site. Without this, `any [T]` silently truncates to `Simple("any", Nil)` and the
    // `[T]` tail dangles — the exact partial-consume failure mode described in D15.
    P(
      CharsWhileIn(" \t", 0) ~ "[" ~/ (
        (anyQualifier ~ ("," ~ typeRef).?).map { case (q, t) => (Some(q): Option[RawTypeRef.AnyRef.Qual], t) } |
        typeRef.map(t => (None: Option[RawTypeRef.AnyRef.Qual], Some(t)))
      ) ~ "]"
    )
  }

  def typeRef[$: P]: P[RawTypeRef] = {
    import fastparse.SingleLineWhitespace.whitespace
    // `any` is syntactically special ONLY when it appears as `any[...]` at the head of a typeRef.
    // Bare `any` and prefixed forms like `foo.any` / `any.Foo` are parsed as ordinary identifiers;
    // the typer decides whether the identifier resolves to the builtin or to a user type.
    //
    // We parse the identifier path atomically first (no backtrack hazards after `"any"` consumes
    // 3 chars), then branch: when the head is exactly `any` (single segment, no prefix), route
    // to `anyTypeRefArgs` if `[` follows; otherwise fall through to the generic shape.
    nonGenericTypeRef.flatMap {
      ref =>
        val name   = ref.path.last.name
        val prefix = ref.path.toList.init

        if (name == "any" && prefix.isEmpty) {
          // Head is bare `any`. If brackets follow, interpret as the any-builtin syntax; otherwise
          // treat as an ordinary identifier.
          anyTypeRefArgs.map { case (q, u) => RawTypeRef.AnyRef(q, u): RawTypeRef } |
          Pass(RawTypeRef.Simple(RawTypeName(name), prefix): RawTypeRef)
        } else {
          typeParams.?.map {
            case Some(value) => RawTypeRef.Constructor(RawTypeName(name), value, prefix)
            case None        => RawTypeRef.Simple(RawTypeName(name), prefix)
          }
        }
    }
  }

  def fieldName[$: P]: P[RawFieldName] =
    idt.symbol.map(name => RawFieldName(name))

  def contractDef[$: P]: P[RawContractRef] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    ("is" ~ nonGenericTypeRef).map { case t => model.RawContractRef(t) }
  }

  def extendedContractRef[$: P]: P[ContractRef] = {
    meta
      .withMeta(contractDef)
      .map { case (meta, ref) => ContractRef(ref, meta) }
  }

  private def fieldWas[$: P]: P[RawFieldName] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    kw.was ~ fieldName
  }

  def fieldDef[$: P]: P[RawField] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    (fieldName ~ ":" ~ typeRef ~ fieldWas.?).map { case (n, t, prev) => model.RawField(n, t, prev) }
  }

  // NOTE: BaboonWhitespace (imported below) consumes newlines, so `nonGenericTypeRef ~ typeParams.?`
  // allows `[…]` on a separate line to silently bind as args. This is safe contingent on no other
  // dtoMember alternative starting with `[`; a future extension that introduces one would break this
  // silently. Pinned by the cross-line binding tripwire test in M33StructuralTemplateInheritanceParserTest.
  def parentDef[$: P]: P[(ScopedRef, Option[NEList[RawTypeRef]])] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    ("+" ~ nonGenericTypeRef ~ typeParams.?).map { case (ref, args) => (ref, args) }
  }

  def unparentDef[$: P]: P[(ScopedRef, Option[NEList[RawTypeRef]])] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    ("-" ~ nonGenericTypeRef ~ typeParams.?).map { case (ref, args) => (ref, args) }
  }

  def intersectionDef[$: P]: P[(ScopedRef, Option[NEList[RawTypeRef]])] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    ("^" ~ nonGenericTypeRef ~ typeParams.?).map { case (ref, args) => (ref, args) }
  }

  def unfieldDef[$: P]: P[RawField] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    "-" ~ fieldDef
  }

  /** Parses `has (mirror | contract) <bare-identifier>` (T37).
    *
    * Grammar (BaboonWhitespace):  `has` keyword — then `mirror` or `contract` keyword — then a
    * bare symbol (no dot-qualified path, no type args).
    *
    * D15 discipline: this alternative does NOT use `/~` (cut) after `kw.has`, so that a field
    * named `has` (e.g. `has: i32`) still backtracks cleanly to `fieldDef`. The order in
    * `dtoMember` places the `meta.withMeta(extractionDef)` branch before `fieldDef` so it is
    * tried first, but the absence of a cut means failure inside `extractionDef` reverts the
    * cursor to before `has`. Fields named `mirror` or `contract` are unaffected — `fieldDef`
    * does not restrict `fieldName` to non-keywords.
    */
  private def extractionDefCore[$: P]: P[(ExtractionKind, RawTypeName)] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    kw.has ~ (kw.mirror.map(_ => ExtractionKind.Mirror: ExtractionKind) | kw.contract.map(_ => ExtractionKind.Contract: ExtractionKind)) ~ idt.symbol.map(RawTypeName.apply)
  }

  def extendedExtractionDef[$: P]: P[RawDtoMember.ExtractionDef] = {
    meta.withMeta(extractionDefCore).map { case (m, (kind, name)) => RawDtoMember.ExtractionDef(kind, name, m) }
  }

  // Field-definition branch with optional postfix `//!` doc capture.
  //
  // The connection between `fieldDef` and `suffixDoc` runs under `NoWhitespace`
  // so that no NLC can be consumed between the field's last token and the `//!`
  // marker -- otherwise `BaboonWhitespace` (which eats `\n`) would let a
  // freestanding `//!` on the next line silently bind to the previous field.
  // `suffixDoc` itself consumes a leading run of `[ \t]*` before the marker.
  // [PR-30.2-D01].
  private def fieldDefWithSuffix[$: P]: P[(RawField, Option[RawDocComment])] = {
    import fastparse.NoWhitespace.noWhitespaceImplicit
    noWhitespaceImplicit.discard()
    P(fieldDef ~ docs.suffixDoc)
  }

  def dtoMember[$: P]: P[RawDtoMember] = {
    // No `~` operators are composed directly inside this body -- each branch's
    // whitespace contract is managed by its own helper (`fieldDefWithSuffix`,
    // `parentDef`, `meta.withMeta(...)`, etc.). The `|` alternation operator
    // does not consume whitespace, so no implicit Whitespace import is needed
    // at this scope.
    P(meta.withMeta(fieldDefWithSuffix)).map {
      case (m, (field, suffix)) =>
        val merged = m.copy(docs = RawDocs(m.docs.prefix, suffix))
        model.RawDtoMember.FieldDef(field, merged)
    } | P(meta.withMeta(parentDef)).map {
      case (meta, (parent, args)) =>
        model.RawDtoMember.ParentDef(parent, meta, args)
    } | P(meta.withMeta(unfieldDef)).map {
      case (meta, field) =>
        model.RawDtoMember.UnfieldDef(field, meta)
    } | P(meta.withMeta(unparentDef)).map {
      case (meta, (parent, args)) =>
        model.RawDtoMember.UnparentDef(parent, meta, args)
    } | P(meta.withMeta(intersectionDef)).map {
      case (meta, (parent, args)) =>
        model.RawDtoMember.IntersectionDef(parent, meta, args)
    } | extendedContractRef | extendedExtractionDef
  }

  def dto[$: P]: P[Seq[RawDtoMember]] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    P(dtoMember.rep())
  }

  def dtoEnclosed[$: P]: P[RawDto] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    P(meta.member(kw.data, templateHead.? ~ meta.derived ~ struct.enclosed(dto))).map {
      case (meta, name, (tps, derived, members)) =>
        RawDto(RawTypeName(name), members, derived, meta, tps.getOrElse(Nil))
    }
  }

  def identifierEnclosed[$: P]: P[RawIdentifier] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    P(meta.member(kw.identifier, templateHead.? ~ meta.derived ~ struct.enclosed(dto))).map {
      case (meta, name, (tps, derived, members)) => RawIdentifier(RawTypeName(name), members, derived, meta, tps.getOrElse(Nil))
    }
  }

}
