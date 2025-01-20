package io.septimalmind.baboon.parser.defns

import fastparse.{EagerOps, P}
import io.septimalmind.baboon.parser.ParserContext
import io.septimalmind.baboon.parser.defns.base.{kw, struct}
import io.septimalmind.baboon.parser.model.{RawContract, RawTypeName}
import izumi.fundamentals.platform.language.Quirks

class DefContract(context: ParserContext, meta: DefMeta, dto: DefDto) {
  Quirks.discard(context)
  def contractEnclosed[$: P]: P[RawContract] = {
    P(meta.member(kw.contract, struct.enclosed(dto.dto))).map {
      case (meta, name, members) =>
        RawContract(RawTypeName(name), members, meta)
    }
  }

}
