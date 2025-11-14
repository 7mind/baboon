package io.septimalmind.baboon.util

import io.circe.Json
import io.septimalmind.baboon.typer.model.{BaboonFamily, Conversion, TypeId}
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
            (pkg.toString, line.evolution.typesUnchangedSince.flatMap(_._2.values).asJson)
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
        "dependencies" -> Json.obj(family.domains.toSeq.map {
          case (pkg, line) =>
            (
              pkg.toString,
              line.versions.toSeq.map(_._2.defs.predecessors.links).asJson,
            )
        }*),
        "identifiers" -> Json.obj(family.domains.toSeq.map {
          case (pkg, line) =>
            (
              pkg.toString,
              line.versions.toSeq.map(v => (v._1, v._2.defs.meta.nodes.keys.toList.collect { case u: TypeId.User => (u.toString, u.render) }.toMap)).toMap.asJson,
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
