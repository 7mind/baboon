package io.septimalmind.baboon.translator.swift

import io.septimalmind.baboon.typer.model.{BaboonEvolution, Domain}

trait SwFileTools {
  def basename(dom: Domain, evolution: BaboonEvolution): String
}

object SwFileTools {
  class SwFileToolsImpl(trans: SwTypeTranslator) extends SwFileTools {
    def basename(dom: Domain, evolution: BaboonEvolution): String = {
      trans.domainModuleName(dom.id, dom.version, evolution)
    }
  }
}
