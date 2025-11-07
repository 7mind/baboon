package io.septimalmind.baboon

import io.septimalmind.baboon.parser.model.FSPath

import java.io.File
import java.nio.file.{Path, Paths}

object PathTools {
  implicit class FsPathExt(path: FSPath) {
    def toFile: File                = Paths.get(path.asString).toFile
    def toPath: Path                = Paths.get(path.asString)
    def resolve(name: String): Path = Paths.get(path.asString).resolve(name)
    def getParent: Path             = Paths.get(path.asString).getParent
  }
}
