package io.septimalmind.baboon.translator

import io.septimalmind.baboon.typer.model.{TypeId, TypeRef}

/** Identifier-repr categories after alias resolution. Target renderers retain
  * their own integer representations and canonical parsing/range checks.
  */
sealed trait IdentifierFieldKind

object IdentifierFieldKind {
  case object Bit extends IdentifierFieldKind
  case object SignedInt extends IdentifierFieldKind /* i08/i16/i32 */
  case object SignedLong extends IdentifierFieldKind /* i64 */
  case object UnsignedSmallInt extends IdentifierFieldKind /* u08/u16/u32 */
  case object UnsignedLong extends IdentifierFieldKind /* u64 */
  case object Str extends IdentifierFieldKind
  case object Uid extends IdentifierFieldKind
  case object Tsu extends IdentifierFieldKind
  case object Tso extends IdentifierFieldKind
  case object Bytes extends IdentifierFieldKind
  final case class NestedId(id: TypeId.User) extends IdentifierFieldKind

  def classify(tpe: TypeRef): IdentifierFieldKind = {
    tpe match {
      case TypeRef.Scalar(b: TypeId.BuiltinScalar) =>
        import TypeId.Builtins.*
        b match {
          case `bit`                 => IdentifierFieldKind.Bit
          case `i08` | `i16` | `i32` => IdentifierFieldKind.SignedInt
          case `i64`                 => IdentifierFieldKind.SignedLong
          case `u08` | `u16` | `u32` => IdentifierFieldKind.UnsignedSmallInt
          case `u64`                 => IdentifierFieldKind.UnsignedLong
          case `str`                 => IdentifierFieldKind.Str
          case `uid`                 => IdentifierFieldKind.Uid
          case `tsu`                 => IdentifierFieldKind.Tsu
          case `tso`                 => IdentifierFieldKind.Tso
          case `bytes`               => IdentifierFieldKind.Bytes
          case other =>
            throw new IllegalStateException(s"Identifier field has unsupported scalar $other; validator should have rejected this.")
        }
      case TypeRef.Scalar(uid: TypeId.User) =>
        IdentifierFieldKind.NestedId(uid)
      case other =>
        throw new IllegalStateException(s"Identifier field has unsupported TypeRef $other; validator should have rejected this.")
    }
  }
}
