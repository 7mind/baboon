package io.septimalmind.baboon.translator.dart

import io.septimalmind.baboon.typer.model.{BaboonLang, TypeId, TypeRef, Typedef}

private[dart] sealed trait DtForeignWirePlan

private[dart] object DtForeignWirePlan {
  final case class Inline(ref: TypeRef) extends DtForeignWirePlan
  case object Codec extends DtForeignWirePlan

  def ueba(foreign: Typedef.Foreign): DtForeignWirePlan = {
    foreign.bindings.get(BaboonLang.Dart) match {
      case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(ref))) => Inline(ref)
      case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.Custom(decl, _))) if decl == "dart.core.String" || decl == "String" =>
        Inline(TypeRef.Scalar(TypeId.Builtins.str))
      case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.Custom(_, _))) =>
        foreign.runtimeMapping.fold[DtForeignWirePlan](Codec)(Inline.apply)
      case _ => Codec
    }
  }
}
