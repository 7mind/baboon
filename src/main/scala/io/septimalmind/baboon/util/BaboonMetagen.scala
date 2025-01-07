package io.septimalmind.baboon.util

import io.circe.Json
import io.septimalmind.baboon.typer.model.{BaboonFamily, Conversion}
import io.circe.syntax.*
import io.septimalmind.baboon.util.BaboonDomainCodecs.*

trait BaboonMetagen {
  def meta(family: BaboonFamily): Json
}

object BaboonMetagen {
  class BaboonMetagenImpl extends BaboonMetagen {
    override def meta(family: BaboonFamily): Json = {

      val data = family.domains.toSeq.flatMap {
        case (_, lineage) =>
          lineage.versions.toSeq.map {
            case (ver, _) =>
              VersionMeta(lineage.pkg.path.mkString("."), ver.version)
          }
      }

      Json.obj(
        "versions" -> data.asJson,
        "unmodified" -> Json.obj(family.domains.toSeq.map {
          case (pkg, line) =>
            (pkg.toString, line.evolution.typesUnchangedSince.asJson)
        }*),
        "underivable" -> Json.obj(family.domains.toSeq.map {
          case (pkg, line) =>
            (
              pkg.toString,
              line.evolution.rules.map {
                case (s, r) =>
                  Json.obj(
                    "to"   -> s.to.asJson,
                    "from" -> s.from.asJson,
                    "failures" -> r.conversions.collect {
                      case c: Conversion.CustomConversionRequired => Json.obj("type" -> c.sourceTpe.toString.asJson, "reason" -> c.reason.asJson)
                    }.asJson,
                  )
              }.asJson,
            )
        }*),
        "recursive" -> Json.obj(family.domains.toSeq.map {
          case (pkg, line) =>
            (
              pkg.toString,
              Json
                .obj(line.versions.map {
                  case (v, d) =>
                    v.version -> Json.obj(
                      d.loops.toSeq.map(l => l.node.toString -> l.loops.map(_.loop.map(_.asJson)).asJson)*
                    )
                }.toSeq*),
            )
        }*),
      )
    }
  }
}
