package io.septimalmind.baboon.translator.rust

import io.septimalmind.baboon.translator.rust.RsValue.RsType
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait RsDomainTreeTools {
  def makeBaboonGeneratedImpl(
    defn: DomainMember.User,
    name: RsType,
    isLatestVersion: Boolean,
  ): TextTree[RsValue]
}

object RsDomainTreeTools {
  final class RsDomainTreeToolsImpl(
    domain: Domain
  ) extends RsDomainTreeTools {

    override def makeBaboonGeneratedImpl(
      defn: DomainMember.User,
      name: RsType,
      isLatestVersion: Boolean,
    ): TextTree[RsValue] = {
      val typeName            = name.asName
      val baboonDomainVersion = domain.version.v.toString
      val baboonDomainId      = defn.id.pkg.toString
      val baboonTypeId        = defn.id.toString

      val mainImpl =
        q"""impl crate::baboon_runtime::BaboonGenerated for $typeName {
           |    fn baboon_domain_version() -> &'static str { "$baboonDomainVersion" }
           |    fn baboon_domain_identifier() -> &'static str { "$baboonDomainId" }
           |    fn baboon_type_identifier() -> &'static str { "$baboonTypeId" }
           |}""".stripMargin

      val latestImpl = if (isLatestVersion) {
        q"""
           |
           |impl crate::baboon_runtime::BaboonGeneratedLatest for $typeName {}""".stripMargin
      } else q""

      val adtImpl = defn.id.owner match {
        case Owner.Adt(_) =>
          q"""
             |
             |impl crate::baboon_runtime::BaboonAdtMemberMeta for $typeName {}""".stripMargin
        case _ => q""
      }

      q"$mainImpl$latestImpl$adtImpl"
    }
  }
}
