package io.septimalmind.baboon.translator.python

import io.septimalmind.baboon.translator.python.PyTypes.*
import io.septimalmind.baboon.typer.model.{BaboonEvolution, Domain, DomainMember, Owner, Typedef}
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.Quote

trait PyDomainTreeTools {
  def makeDataMeta(defn: DomainMember.User): Seq[TextTree[PyValue]]
  def makeCodecMeta(defn: DomainMember.User): Seq[TextTree[PyValue]]
}

object PyDomainTreeTools {
  final class PyDomainTreeToolsImpl(
    domain: Domain,
    evolution: BaboonEvolution,
    typeTranslator: PyTypeTranslator,
    domainTypes: PyDomainTypes,
    pyFileTools: PyFileTools,
  ) extends PyDomainTreeTools {
    override def makeDataMeta(defn: DomainMember.User): Seq[TextTree[PyValue]] = {
      makeFullMeta(defn, isCodec = false)
    }

    override def makeCodecMeta(defn: DomainMember.User): Seq[TextTree[PyValue]] = {
      defn.defn match {
        case _: Typedef.Enum    => makeFullMeta(defn, isCodec = true)
        case _: Typedef.Foreign => makeFullMeta(defn, isCodec = true)
        case _                  => makeRefMeta(defn)
      }
    }

    private def makeFullMeta(defn: DomainMember.User, isCodec: Boolean): Seq[TextTree[PyValue]] = {
      val adtMethods = defn.id.owner match {
        case Owner.Adt(id) =>
          List(
            q"""baboon_adt_type_identifier: $pyClassVar[$pyStr] = "${id.toString}"
               |@$pyClassMethod
               |def baboon_adt_type(cls) -> $pyType:
               |    return cls
               |""".stripMargin
          )
        case _ => List.empty
      }

      val unmodifiedMethods = if (!isCodec) {
        val unmodifiedSince = evolution.typesUnchangedSince(domain.version)(defn.id)
        val forward         = evolution.typesForwardReadable(domain.version)(defn.id)
        val forwardEntries  = forward.readable.toList.map { case (v, tier) => s""""${v.v.toString}": "${tier.wireName}"""" }.mkString(", ")
        val minReaderEntries = evolution.minReaders(domain.version, defn.id).toList.sortBy(_._1.weight)
          .map { case (tier, v) => s""""${tier.wireName}": "${v.v.toString}"""" }.mkString(", ")
        List(
          q"""baboon_same_in_versions: $pyClassVar[$pyList[$pyStr]] = [${unmodifiedSince.sameIn.map(v => q"\"${v.v.toString}\"").toList.join(", ")}]""".stripMargin,
          q"""baboon_forward_readable: $pyClassVar[$pyDict[$pyStr, $pyStr]] = {$forwardEntries}""".stripMargin,
          q"""baboon_min_reader_versions: $pyClassVar[$pyDict[$pyStr, $pyStr]] = {$minReaderEntries}""".stripMargin,
        )
      } else Nil

      domainAndTypeMeta(defn) ++ unmodifiedMethods ++ adtMethods
    }

    private def makeRefMeta(defn: DomainMember.User): Seq[TextTree[PyValue]] = {
      val pyType = domainTypes.asPyType(defn.id, pyFileTools.definitionsBasePkg)

      val adtMethods = defn.id.owner match {
        case Owner.Adt(id) =>
          List(
            q"""baboon_adt_type_identifier: $pyClassVar[$pyStr] = $pyType.baboon_adt_type_identifier
               |@$pyClassMethod
               |def baboon_adt_type(cls) -> type:
               |    return type(${domainTypes.asPyType(id, pyFileTools.definitionsBasePkg)})
               |""".stripMargin
          )
        case _ => Nil
      }

      domainAndTypeMeta(defn) ++ adtMethods
    }

    private def domainAndTypeMeta(defn: DomainMember.User): Seq[TextTree[PyValue]] = {
      Seq(
        q"""baboon_domain_version: $pyClassVar[$pyStr] = "${domain.version.v.toString}"""",
        q"""baboon_domain_identifier: $pyClassVar[$pyStr] = "${defn.id.pkg.toString}"""",
        q"""baboon_type_identifier: $pyClassVar[$pyStr] = "${defn.id.toString}"""",
      )
    }
  }
}
