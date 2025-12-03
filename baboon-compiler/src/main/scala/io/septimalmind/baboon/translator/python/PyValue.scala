package io.septimalmind.baboon.translator.python

import io.septimalmind.baboon.typer.model.Version
import izumi.fundamentals.collections.nonempty.NEList

sealed trait PyValue
object PyValue {
  final case class PyModuleId(path: NEList[String], version: Option[Version] = None) {
    val module: String                      = path.last
    val moduleVersionString: Option[String] = version.map(v => v.format(prefix = "v", delimiter = "_"))
    val pathToVersion: List[String]         = path.toList.takeWhile(p => !moduleVersionString.contains(p))
    def withModuleName(name: String): PyModuleId = {
      this.copy(path = NEList.unsafeFrom(path.toList.init :+ name))
    }
    val isBaboonModule: Boolean = path.head.startsWith("baboon")
  }
  object PyModuleId {
    def apply(module: String): PyModuleId = {
      new PyModuleId(NEList.unsafeFrom(List(module)), None)
    }
  }

  final case class PyType(moduleId: PyModuleId, name: String, versioned: Boolean = false) extends PyValue
}
