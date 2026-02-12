package io.septimalmind.baboon.translator.typescript

import io.septimalmind.baboon.typer.model.Version

trait TsValue
object TsValue {
  final case class TsModuleId(path: List[String], version: Option[Version] = None)
  object TsModuleId {
    def apply(module: String): TsModuleId = {
      new TsModuleId(List(module))
    }
  }
  final case class TsType(
    moduleId: TsModuleId,
    name: String,
    alias: Option[String] = None,
    predef: Boolean       = false,
  ) extends TsValue {
    def withAlias(alias: String): TsType = {
      this.copy(alias = Some(alias))
    }
  }
  object TsType {
    def predef(typeName: String): TsType = {
      new TsType(TsModuleId(""), typeName, predef = true)
    }
  }
}
