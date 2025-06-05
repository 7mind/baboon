package io.septimalmind.baboon.translator.scl

import io.septimalmind.baboon.typer.model.TypeId

class ScTypeInfo() {
  def adtNsName(id: TypeId.User): String = {
    id.name.name
  }
}
