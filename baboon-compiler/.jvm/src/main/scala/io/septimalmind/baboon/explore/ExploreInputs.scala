package io.septimalmind.baboon.explore

import io.septimalmind.baboon.parser.model.FSPath

final case class ExploreInputs(
  directoryInputs: Set[FSPath],
  individualInputs: Set[FSPath],
)
