package io.septimalmind.baboon.translator

import io.septimalmind.baboon.typer.model.{TypeId, TypeRef}

sealed trait ValueConversionPlan

object ValueConversionPlan {
  final case class Scalar(source: TypeRef.Scalar, target: TypeRef.Scalar) extends ValueConversionPlan
  case object CopyOpaque extends ValueConversionPlan
  final case class WrapOptional(element: ValueConversionPlan) extends ValueConversionPlan
  final case class WrapList(element: ValueConversionPlan) extends ValueConversionPlan
  final case class WrapSet(element: ValueConversionPlan) extends ValueConversionPlan
  final case class MapOptional(element: ValueConversionPlan) extends ValueConversionPlan
  final case class MapList(element: ValueConversionPlan) extends ValueConversionPlan
  final case class MapSet(element: ValueConversionPlan) extends ValueConversionPlan
  final case class MapEntries(key: ValueConversionPlan, value: ValueConversionPlan) extends ValueConversionPlan

  def apply(source: TypeRef, target: TypeRef): ValueConversionPlan = (source, target) match {
    case (s: TypeRef.Scalar, t: TypeRef.Scalar) => Scalar(s, t)
    case (s: TypeRef.Scalar, t: TypeRef.Constructor) =>
      val element = apply(s, t.args.head)
      t.id match {
        case TypeId.Builtins.opt => WrapOptional(element)
        case TypeId.Builtins.lst => WrapList(element)
        case TypeId.Builtins.set => WrapSet(element)
        case _                  => throw new IllegalStateException(s"Unsupported constructor type: ${t.id}")
      }
    case (s: TypeRef.Constructor, _: TypeRef.Scalar) =>
      throw new IllegalStateException(s"Unsupported scalar to constructor conversion: ${s.id}")
    case (s: TypeRef.Constructor, t: TypeRef.Constructor) =>
      t.id match {
        case TypeId.Builtins.opt => MapOptional(apply(s.args.head, t.args.head))
        case TypeId.Builtins.lst => MapList(apply(s.args.head, t.args.head))
        case TypeId.Builtins.set => MapSet(apply(s.args.head, t.args.head))
        case TypeId.Builtins.map => MapEntries(apply(s.args.head, t.args.head), apply(s.args.last, t.args.last))
        case _                  => throw new IllegalStateException(s"Unsupported constructor type: ${t.id}")
      }
    // `any` payload is opaque; we never auto-convert between `any` and a non-`any`
    // type (validator forbids it). Identical (variant, underlying) pairs copy the
    // reference as-is: the wire bytes carry their own meta header, independent of
    // the containing DTO's schema version. Variant or underlying changes are
    // breaking per spec §Evolution and should have been rejected by
    // `BaboonRules.incompatibleAdditions`; retain the defensive invariant checks.
    case (s: TypeRef.Any, t: TypeRef.Any) =>
      if (s == t) CopyOpaque
      else
        throw new IllegalStateException(
          s"BUG: conversion of `any` field across variant or underlying change is breaking and should have been rejected by BaboonRules.incompatibleAdditions: $s -> $t"
        )
    case (other, _: TypeRef.Any) =>
      throw new IllegalStateException(s"BUG: cannot auto-convert field of type $other to `any` (not allowed by evolution rules)")
    case (_: TypeRef.Any, other) =>
      throw new IllegalStateException(s"BUG: cannot auto-convert `any` field to type $other (not allowed by evolution rules)")
  }
}
