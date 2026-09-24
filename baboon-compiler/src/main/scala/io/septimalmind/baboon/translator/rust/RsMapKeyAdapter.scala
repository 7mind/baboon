package io.septimalmind.baboon.translator.rust

import io.septimalmind.baboon.translator.rust.RsDefnTranslator.toSnakeCase
import io.septimalmind.baboon.typer.model.*

/** Where a `map[K, V]` with a user-typed key gets its JSON string keys from.
  *
  * serde_json rejects non-string-shaped map keys, so the definition translator emits a
  * per-key-type adapter module beside K's struct and points the field at it with
  * `#[serde(with = ...)]`. The adapter is the only thing that knows how each eligible key kind
  * converts: an `id` type has `Display` but deliberately no `FromStr` (its reader is the free
  * `parse_repr`), a single-primitive wrapper has neither and is peeled to its inner scalar, and
  * an enum has both.
  *
  * The explicit JSON codecs need the same answer, so the lookup lives here rather than being
  * duplicated — guessing `to_string()` / `parse::<K>()` compiles for enums and for nothing else.
  */
final class RsMapKeyAdapter(domain: Domain, domainTypes: RsDomainTypes) {
  /** PR-61 (M19.3): if `f` is a top-level `map[K, V]` whose K is an eligible user
    * key, return the FQ Rust path of the adapter module sibling to K's struct.
    * Eligibility mirrors PR-59's validator (`isEligibleKey`): id types are eligible
    * unconditionally; non-id `data` requires single-primitive-field shape with no
    * contracts. We assume the validator has already rejected ineligible cases —
    * here we only decide whether to attach the `#[serde(with = ...)]` attribute.
    *
    * Limited to the top-level `map[K, V]` shape (no `Option<BTreeMap<K, V>>` or
    * nested `BTreeMap<K1, BTreeMap<K2, V>>` — adapter modules are field-scoped
    * and serde's `with` attribute is applied to the field's full type).
    */
  def userMapKeyAdapterPath(tpe: TypeRef): Option[String] = tpe match {
    case TypeRef.Constructor(TypeId.Builtins.map, args) => keyAdapterPathFor(args.head)
    case _                                              => None
  }

  /** Same lookup against a bare key type, for callers that build the map themselves and need
    * only the key conversion.
    */
  def keyAdapterPathFor(keyTpe: TypeRef): Option[String] = keyTpe match {
    case TypeRef.Scalar(uid: TypeId.User) =>
      domain.defs.meta.nodes.get(uid) match {
        case Some(DomainMember.User(_, dto: Typedef.Dto, _, _)) if isUserMapKeyEligibleDto(dto) =>
          val rsT     = domainTypes.toRsTypeRefKeepForeigns(uid)
          val modName = s"${toSnakeCase(rsT.name)}_as_map_key"
          Some((rsT.crate.parts.toSeq :+ modName).mkString("::"))
        // PR-I.3 (M24 Phase 3.3): direct foreign map key — route through
        // the foreign's emitted `<foreign>_as_map_key` adapter (Custom only;
        // BaboonRef-aliased foreigns reuse the aliased type's serde path).
        case Some(DomainMember.User(_, f: Typedef.Foreign, _, _)) =>
          f.bindings.get(BaboonLang.Rust) match {
            case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.Custom(_, _))) =>
              val rsT     = domainTypes.toRsTypeRefKeepForeigns(uid)
              val modName = s"${toSnakeCase(rsT.name)}_as_map_key"
              Some((rsT.crate.parts.toSeq :+ modName).mkString("::"))
            case _ => None
          }
        case _ => None
      }
    case _ => None
  }

  /** Mirrors `BaboonValidator.isEligibleKey` — DTO, Enum, and Foreign branches.
    * id types are always eligible (Q-M19-6); single-primitive-field non-contract DTOs are
    * eligible if the inner field is a primitive scalar, an enum, a foreign type, or a
    * recursively-eligible nested wrapper (no opt/collection, no float wrappers per Q-M19-2).
    */
  def isUserMapKeyEligibleDto(dto: Typedef.Dto): Boolean = {
    if (dto.isIdentifier) true
    else if (dto.contracts.nonEmpty) false
    else if (dto.fields.size != 1) false
    else
      dto.fields.head.tpe match {
        case _: TypeRef.Constructor => false
        case TypeRef.Scalar(b: TypeId.BuiltinScalar) =>
          b match {
            case TypeId.Builtins.f32 | TypeId.Builtins.f64 | TypeId.Builtins.f128 => false
            case _                                                                => true
          }
        case TypeRef.Scalar(u: TypeId.User) =>
          domain.defs.meta.nodes.get(u) match {
            case Some(DomainMember.User(_, nested: Typedef.Dto, _, _)) => isUserMapKeyEligibleDto(nested)
            // Q-M19-7: enums round-trip via Display / parse; foreign types assume the host
            // provides compatible Display / FromStr (PR-I will route through a dedicated KeyCodec).
            case Some(DomainMember.User(_, _: Typedef.Enum, _, _))    => true
            case Some(DomainMember.User(_, _: Typedef.Foreign, _, _)) => true
            case _                                                    => false
          }
        case _ => false
      }
  }
}
