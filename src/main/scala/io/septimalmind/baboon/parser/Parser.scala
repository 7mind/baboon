package io.septimalmind.baboon.parser

import fastparse.Parsed
import io.septimalmind.baboon.parser.model.RawDomain

class Parser(context: ParserContext) {

  def parse(): Parsed[RawDomain] = {
    fastparse.parse(context.content, context.defModel.model(_))
  }
}
