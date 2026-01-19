package io.septimalmind.baboon.util

import io.septimalmind.baboon.parser.model.FSPath

trait FileContentProvider {
  def read(path: FSPath): Option[String]
}
