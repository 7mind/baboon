package io.septimalmind.baboon.parser.defns

import fastparse.{EagerOps, P}
import io.septimalmind.baboon.parser.ParserContext
import io.septimalmind.baboon.parser.defns.base.{kw, struct}
import io.septimalmind.baboon.parser.model.{RawContract, RawTypeName}
import izumi.fundamentals.platform.language.Quirks

class DefContract(context: ParserContext, meta: DefMeta, dto: DefDto) {
  Quirks.discard(context)
  def contractEnclosed[$: P]: P[RawContract] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    P(meta.member(kw.contract, dto.templateHead.? ~ struct.enclosed(dto.dto))).map {
      case (meta, name, (tps, members)) =>
        RawContract(RawTypeName(name), members, meta, tps.getOrElse(Nil))
    }
  }
}
