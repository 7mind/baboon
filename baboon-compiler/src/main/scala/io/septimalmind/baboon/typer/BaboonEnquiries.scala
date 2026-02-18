package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.model.{RawAdt, RawDefn, RawDtoMember, RawDtoid, ScopedRef}
import io.septimalmind.baboon.typer.model.BinReprLen.Variable
import io.septimalmind.baboon.typer.model.TypeId.Builtins
import io.septimalmind.baboon.typer.model.Typedef.{Contract, ForeignMapping}
import io.septimalmind.baboon.typer.model.{BaboonLang, BinReprLen, Domain, DomainMember, Field, Owner, ShallowSchemaId, TypeId, TypeRef, Typedef}
import izumi.fundamentals.collections.nonempty.NESet
import izumi.fundamentals.graphs.struct.AdjacencyList
import izumi.fundamentals.graphs.tools.cycles.LoopDetector
import izumi.fundamentals.platform.crypto.IzSha256HashFunction

import scala.annotation.tailrec
import scala.collection.mutable

sealed trait ForeignResolution
object ForeignResolution {
  case class CustomType(entry: Typedef.ForeignEntry) extends ForeignResolution
  case class BaboonAlias(typeRef: TypeRef) extends ForeignResolution
}

trait BaboonEnquiries {
  def fullDepsOfDefn(defn: DomainMember): Set[TypeId]
  def allRefs(defn: DomainMember): Set[TypeRef]
  def wrap(id: TypeId): String
  def explode(tpe: TypeRef): Set[TypeId]
  def shallowId(defn: DomainMember): ShallowSchemaId
  def hardDepsOfRawDefn(dd: RawDefn): Set[ScopedRef]
  def hasForeignType(definition: DomainMember.User, domain: Domain): Boolean
  def hasForeignType(definition: DomainMember.User, domain: Domain, lang: BaboonLang): Boolean
  def resolveForeignBinding(defn: Typedef.Foreign, lang: BaboonLang): Option[ForeignResolution]
  def isRecursiveTypedef(definition: DomainMember.User, domain: Domain): Boolean
  def loopsOf(
    domain: Map[TypeId, DomainMember]
  ): Set[LoopDetector.Cycles[TypeId]]
  def uebaLen(dom: Map[TypeId, DomainMember], tpe: TypeRef): BinReprLen
  def isEnum(tpe: TypeRef, domain: Domain): Boolean

  def unfold(dom: Domain, contracts: List[TypeId.User]): List[Field]

  def collectParents(domain: Domain, definitions: List[DomainMember.User]): List[TypeId.User]
}

object BaboonEnquiries {

  def isBaboonRefForeign(id: TypeId, domain: Domain, lang: BaboonLang): Boolean = {
    domain.defs.meta.nodes.get(id).exists {
      case DomainMember.User(_, f: Typedef.Foreign, _, _) =>
        f.bindings.get(lang).exists(_.mapping.isInstanceOf[Typedef.ForeignMapping.BaboonRef])
      case _ => false
    }
  }

  def resolveBaboonRef(tpe: TypeRef, domain: Domain, lang: BaboonLang): TypeRef = {
    tpe match {
      case TypeRef.Scalar(id: TypeId.User) =>
        domain.defs.meta.nodes.get(id) match {
          case Some(DomainMember.User(_, f: Typedef.Foreign, _, _)) =>
            f.bindings.get(lang) match {
              case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                resolveBaboonRef(aliasedRef, domain, lang)
              case _ => tpe
            }
          case _ => tpe
        }
      case TypeRef.Constructor(id, args) =>
        TypeRef.Constructor(id, args.map(a => resolveBaboonRef(a, domain, lang)))
      case _ => tpe
    }
  }

  class BaboonEnquiriesImpl extends BaboonEnquiries {
    def unfold(dom: Domain, contracts: List[TypeId.User]): List[Field] = {
      val direct = contracts
        .map(id => dom.defs.meta.nodes(id))
        .collect { case c: DomainMember.User => c.defn }
        .collect { case c: Contract => c }
      val parents = direct.flatMap(c => unfold(dom, c.contracts))
      (parents ++ direct.flatMap(_.fields)).distinct
    }

    def loopsOf(
      domain: Map[TypeId, DomainMember]
    ): Set[LoopDetector.Cycles[TypeId]] = {
      val depMatrix = AdjacencyList(domain.view.mapValues {
        defn =>
          fullDepsOfDefn(defn)
      }.toMap)

      val loops =
        LoopDetector.Impl.findCyclesForNodes(depMatrix.links.keySet, depMatrix)

      loops
    }

    def resolveForeignBinding(defn: Typedef.Foreign, lang: BaboonLang): Option[ForeignResolution] = {
      defn.bindings.get(lang).map { entry =>
        entry.mapping match {
          case ForeignMapping.BaboonRef(typeRef) => ForeignResolution.BaboonAlias(typeRef)
          case ForeignMapping.Custom(_, _)       => ForeignResolution.CustomType(entry)
        }
      }
    }

    def hasForeignType(definition: DomainMember.User, domain: Domain, lang: BaboonLang): Boolean = {
      hasForeignTypeImpl(definition, domain, Some(lang))
    }

    def hasForeignType(definition: DomainMember.User, domain: Domain): Boolean = {
      hasForeignTypeImpl(definition, domain, None)
    }

    private def hasForeignTypeImpl(definition: DomainMember.User, domain: Domain, lang: Option[BaboonLang]): Boolean = {
      def processRefs(foreignType: Option[TypeId], tail: List[Typedef], refs: List[TypeRef], seen: mutable.HashSet[TypeId]): Option[TypeId] = {
        val moreToCheck = refs.flatMap {
          case TypeRef.Scalar(id) =>
            List(domain.defs.meta.nodes(id) match {
              case _: DomainMember.Builtin => None
              case u: DomainMember.User    => Some(u.defn)
            }).flatten
          case TypeRef.Constructor(_, args) =>
            args
              .map(_.id)
              .map(domain.defs.meta.nodes(_))
              .toList
              .flatMap {
                case _: DomainMember.Builtin => None
                case u: DomainMember.User    => Some(u.defn)
              }
        }
        collectForeignType(tail ++ moreToCheck, foreignType, seen)
      }

      @tailrec
      def collectForeignType(toCheck: List[Typedef], foreignType: Option[TypeId], seen: mutable.HashSet[TypeId]): Option[TypeId] = {
        (toCheck.filterNot(d => seen.contains(d.id)), foreignType) match {
          case (_, Some(tpe)) =>
            seen += tpe
            Some(tpe)
          case (Nil, fType) =>
            seen ++= fType
            fType
          case (head :: tail, None) =>
            seen += head.id
            head match {
              case dto: Typedef.Dto =>
                processRefs(foreignType, tail, dto.fields.map(_.tpe), seen)
              case c: Typedef.Contract =>
                processRefs(foreignType, tail, c.fields.map(_.tpe), seen)
              case adt: Typedef.Adt =>
                val dtos = adt.members
                  .map(tpeId => domain.defs.meta.nodes(tpeId))
                  .toList
                  .collect {
                    case DomainMember.User(_, dto: Typedef.Dto, _, _)      => dto
                    case DomainMember.User(_, dto: Typedef.Contract, _, _) => dto
                  }
                collectForeignType(tail ++ dtos, foreignType, seen)
              case f: Typedef.Foreign =>
                lang.flatMap(l => f.bindings.get(l)) match {
                  case Some(Typedef.ForeignEntry(_, ForeignMapping.BaboonRef(typeRef))) =>
                    processRefs(foreignType, tail, List(typeRef), seen)
                  case _ =>
                    collectForeignType(tail, Some(f.id), seen)
                }
              case _: Typedef.Enum =>
                collectForeignType(tail, foreignType, seen)
              case s: Typedef.Service =>
                processRefs(foreignType, tail, s.methods.flatMap(m => Set(m.sig) ++ m.out ++ m.err), seen)
            }
        }
      }

      collectForeignType(
        List(definition.defn),
        None,
        mutable.HashSet.empty[TypeId],
      ).nonEmpty
    }

    override def isRecursiveTypedef(definition: DomainMember.User, domain: Domain): Boolean = {
      domain.loops.exists(_.loops.exists(_.loop.contains(definition.id)))
    }

    def hardDepsOfRawDefn(dd: RawDefn): Set[ScopedRef] = {
      dd match {
        case d: RawDtoid =>
          d.members.collect {
            case d: RawDtoMember.ParentDef =>
              Seq(d.parent)
            case d: RawDtoMember.UnparentDef =>
              Seq(d.parent)
            case d: RawDtoMember.IntersectionDef =>
              Seq(d.parent)
            case d: RawDtoMember.ContractRef =>
              Seq(d.contract.tpe)
            case _ =>
              Seq.empty
          }.flatten.toSet
        case a: RawAdt =>
          a.contracts.map(_.contract.tpe).toSet
        case _ =>
          Set.empty
      }
    }

    /**
      * Includes soft dependencies (fields)
      */
    def fullDepsOfDefn(defn: DomainMember): Set[TypeId] = {
      depsOfDefn(defn, explode)
    }

    private def depsOfDefn(defn: DomainMember, explodeRef: TypeRef => Set[TypeId]): Set[TypeId] = {
      def explodeFields(f: List[Field]) = {
        f.flatMap(f => explodeRef(f.tpe)).toSet
      }

      // TODO: do we REALLY need to consider field types as dependencies?
      defn match {
        case _: DomainMember.Builtin =>
          Set.empty
        case u: DomainMember.User =>
          u.defn match {
            case t: Typedef.Dto =>
              explodeFields(t.fields) ++ t.contracts
            case t: Typedef.Contract =>
              explodeFields(t.fields) ++ t.contracts
            case _: Typedef.Enum    => Set.empty
            case t: Typedef.Adt     => t.members.toSet ++ t.contracts
            case f: Typedef.Foreign =>
              f.bindings.values.flatMap { entry =>
                entry.mapping match {
                  case ForeignMapping.BaboonRef(typeRef) => explodeRef(typeRef)
                  case ForeignMapping.Custom(_, _)       => Set.empty
                }
              }.toSet
            case s: Typedef.Service =>
              s.methods.flatMap(m => Set(m.sig) ++ m.out.toSet ++ m.err.toSet).flatMap(explodeRef).toSet
          }
      }
    }

    def explode(tpe: TypeRef): Set[TypeId] = {
      val seen = mutable.HashSet.empty[TypeId]
      def doExplode(tpe: TypeRef): Set[TypeId] = tpe match {
        case TypeRef.Scalar(id) =>
          seen += id
          Set(id)
        case TypeRef.Constructor(id, args) if seen.contains(id) =>
          args.toList.flatMap(a => explode(a)).toSet
        case TypeRef.Constructor(id, args) =>
          seen += id
          Set(id) ++ args.toList.flatMap(a => explode(a))
      }

      doExplode(tpe)
    }

    def shallowId(defn: DomainMember): ShallowSchemaId = {
      val normalizedRepr = defn match {
        case id: DomainMember.Builtin => s"[builtin:${id.id.name.name}]"
        case u: DomainMember.User =>
          u.defn match {
            case d: Typedef.Dto =>
              val members = d.fields.map {
                f =>
                  s"${f.name}:${wrap(f.tpe)}"
              }.sorted

              s"[dto;${wrap(d.id)};$members]"
            case d: Typedef.Contract =>
              val members = d.fields.map {
                f =>
                  s"${f.name}:${wrap(f.tpe)}"
              }.sorted
              s"[contract;${wrap(d.id)};$members]"
            case c: Typedef.Enum =>
              val members = c.members.toList.map(_.name).sorted.mkString(",")
              s"[enum;${wrap(c.id)};$members]"
            case c: Typedef.Service =>
              val members = c.methods.map(m => s"[${m.name.name}:${wrap(m.sig)}:${m.out.map(wrap).getOrElse("")}:${m.err.map(wrap).getOrElse("")}]").sorted.mkString(",")
              s"[service;${wrap(c.id)};$members]"
            case a: Typedef.Adt =>
              val members =
                a.members.toList.map(id => wrap(id)).sorted.mkString(",")
              s"[adt;${wrap(a.id)};$members]"
            case f: Typedef.Foreign =>
              val members = f.bindings.values.toList
                .sortBy(_.lang.asString)
                .map { e =>
                  e.mapping match {
                    case ForeignMapping.Custom(decl, attrs) =>
                      s"[${e.lang.asString};$decl;${attrs.attrs.map(a => s"${a.name};${a.value}").sorted.mkString(":")}]"
                    case ForeignMapping.BaboonRef(typeRef) =>
                      s"[${e.lang.asString};baboon:${typeRef.toString}]"
                  }
                }
                .mkString(":")
              s"[foreign;${wrap(f.id)};$members]"
          }
      }

      ShallowSchemaId(IzSha256HashFunction.hash(normalizedRepr))
    }

    def wrap(id: TypeId): String = {
      id match {
        case b: TypeId.Builtin =>
          s"#${b.name.name}"
        case u: TypeId.User =>
          val owner = u.owner match {
            case Owner.Toplevel => "//"
            case Owner.Ns(path) => s"//${path.map(_.name).mkString("/")}/"
            case Owner.Adt(id)  => s"/${wrap(id)}/"
          }
          s"${u.pkg.path.mkString(".")}#$owner#${u.name.name}"

      }

    }

    private def wrap(tpe: TypeRef): String = tpe match {
      case s: TypeRef.Scalar =>
        s"{${wrap(s.id)}}"
      case c: TypeRef.Constructor =>
        s"{${wrap(c.id)}${c.args.toList.map(wrap).mkString("[", ",", "]")}"
    }

    override def allRefs(defn: DomainMember): Set[TypeRef] = {
      defn match {
        case bi: DomainMember.Builtin =>
          bi.id match {
            case s: TypeId.BuiltinScalar =>
              Set(TypeRef.Scalar(s))
            case _: TypeId.BuiltinCollection =>
              Set.empty
          }
        case u: DomainMember.User =>
          val selfref = Set(TypeRef.Scalar(u.id))
          val content = u.defn match {
            case d: Typedef.Dto =>
              d.fields.map(_.tpe).toSet
            case d: Typedef.Contract =>
              d.fields.map(_.tpe).toSet
            case _: Typedef.Enum =>
              Set.empty
            case _: Typedef.Adt =>
              Set.empty
            case f: Typedef.Foreign =>
              f.bindings.values.flatMap { entry =>
                entry.mapping match {
                  case ForeignMapping.BaboonRef(typeRef) => Set(typeRef)
                  case ForeignMapping.Custom(_, _)       => Set.empty[TypeRef]
                }
              }.toSet
            case s: Typedef.Service =>
              s.methods.flatMap(m => Set(m.sig) ++ m.out.toSet ++ m.err.toSet)
          }

          selfref ++ content
      }
    }

    def uebaLen(dom: Map[TypeId, DomainMember], tpe: TypeRef): BinReprLen = {
      binReprLenImpl(dom, tpe, Set.empty)
    }
    private def binReprLenImpl(dom: Map[TypeId, DomainMember], tpe: TypeRef, visited: Set[TypeRef]): BinReprLen = {
      if (visited.contains(tpe)) {
        BinReprLen.Unknown()
      } else {
        tpe match {
          case id: TypeRef.Scalar =>
            scalarLen(dom, id, visited + tpe)

          case TypeRef.Constructor(id, args) =>
            id match {
              case Builtins.map => BinReprLen.Range(1, None)
              case Builtins.lst => BinReprLen.Range(1, None)
              case Builtins.set => BinReprLen.Range(1, None)
              case Builtins.opt =>
                binReprLenImpl(dom, args.head, visited + tpe) match {
                  case BinReprLen.Fixed(bytes) =>
                    BinReprLen.Alternatives(NESet(1, 1 + bytes))
                  case variable: Variable =>
                    variable match {
                      case BinReprLen.Unknown() =>
                        BinReprLen.Unknown()
                      case a: BinReprLen.Alternatives =>
                        a.prefixed(1)
                      case r: BinReprLen.Range =>
                        // null case: 1 byte (bool marker); non-null: 1 + inner bytes
                        BinReprLen.Range(1, r.max.map(_ + 1))
                    }
                }
              // BinReprLen.Alternatives(Set(1, )) // N or 1...
              case u =>
                throw new IllegalStateException(
                  s"BUG: unknown collection type $u"
                )
            }
        }
      }

    }

    private def scalarLen(dom: Map[TypeId, DomainMember], id: TypeRef.Scalar, visited: Set[TypeRef]): BinReprLen = {
      id.id match {
        case id: TypeId.BuiltinScalar =>
          id match {
            case Builtins.bit   => BinReprLen.Fixed(1)
            case Builtins.i08   => BinReprLen.Fixed(1)
            case Builtins.i16   => BinReprLen.Fixed(2)
            case Builtins.i32   => BinReprLen.Fixed(4)
            case Builtins.i64   => BinReprLen.Fixed(8)
            case Builtins.u08   => BinReprLen.Fixed(1)
            case Builtins.u16   => BinReprLen.Fixed(2)
            case Builtins.u32   => BinReprLen.Fixed(4)
            case Builtins.u64   => BinReprLen.Fixed(8)
            case Builtins.f32   => BinReprLen.Fixed(4)
            case Builtins.f64   => BinReprLen.Fixed(8)
            case Builtins.f128  => BinReprLen.Fixed(16)
            case Builtins.str   => BinReprLen.Range(1, None)
            case Builtins.bytes => BinReprLen.Range(1, None)
            case Builtins.uid   => BinReprLen.Fixed(16)
            case Builtins.tsu   => BinReprLen.Fixed(17)
            case Builtins.tso   => BinReprLen.Fixed(17)
            case u =>
              throw new IllegalStateException(s"BUG: unknown scalar type $u")
          }

        case uid: TypeId.User =>
          dom(uid) match {
            case DomainMember.Builtin(id) =>
              throw new IllegalStateException(
                s"BUG: user type id $uid returned builtin type $id"
              )

            case u: DomainMember.User =>
              u.defn match {
                case d: Typedef.Dto =>
                  aggregateLen(dom, d.fields, visited)
                case d: Typedef.Contract =>
                  // contracts should never be serialized,
                  // but if we imagine they do,
                  // the logic will be the same as for dtos
                  aggregateLen(dom, d.fields, visited)
                case _: Typedef.Enum =>
                  BinReprLen.Fixed(1)
                case d: Typedef.Adt =>
                  val nested = d.members
                    .map(id => binReprLenImpl(dom, TypeRef.Scalar(id), visited))
                    .toSet
                  if (nested.size == 1) {
                    nested.head.prefixed(1)
                  } else {
                    // we might consider more cases and use Alternatives/define Max when possible
                    BinReprLen.Range(1, None)
                  }
                case _: Typedef.Service =>
                  // services cannot be serialized at all
                  BinReprLen.Unknown()
                case _: Typedef.Foreign =>
                  BinReprLen.Unknown()
              }
          }

      }
    }

    private def aggregateLen(dom: Map[TypeId, DomainMember], fields: List[Field], visited: Set[TypeRef]): BinReprLen = {
      val binLens = fields.map {
        f =>
          binReprLenImpl(dom, f.tpe, visited)
      }

      val fixed = binLens.collect {
        case BinReprLen.Fixed(s) => s
      }

      if (fixed.size == fields.size) {
        BinReprLen.Fixed(fixed.sum).prefixed(1) // header byte
      } else {
        // we might consider more cases and use Alternatives/define Max when possible
        BinReprLen.Range(1, None)
      }
    }

    def isEnum(tpe: TypeRef, domain: Domain): Boolean = {
      domain.defs.meta.nodes.get(tpe.id).exists {
        case DomainMember.User(_, _: Typedef.Enum, _, _) => true
        case _                                           => false
      }
    }

    override def collectParents(domain: Domain, definitions: List[DomainMember.User]): List[TypeId.User] = {
      def collectUserDefinitions(ids: List[TypeId.User]): List[DomainMember.User] = {
        ids.flatMap(domain.defs.meta.nodes.get).collect { case u: DomainMember.User => u }
      }

      @tailrec
      def loop(acc: List[TypeId.User], toProcess: List[DomainMember.User]): List[TypeId.User] = {
        toProcess match {
          case Nil => acc
          case head :: tail =>
            head.defn match {
              case d: Typedef.Dto =>
                val adtParent = d.id.owner match {
                  case Owner.Adt(id) => collectUserDefinitions(List(id))
                  case _             => List.empty
                }
                val contractsDefs = collectUserDefinitions(d.contracts)
                val all           = adtParent.map(_.id) ++ contractsDefs.map(_.id)
                loop(acc ++ all, tail ++ adtParent ++ contractsDefs)

              case adt: Typedef.Adt =>
                val contractsDefs = collectUserDefinitions(adt.contracts)
                loop(acc ++ contractsDefs.map(_.id), tail ++ contractsDefs)

              case c: Typedef.Contract =>
                val contractsDefs = collectUserDefinitions(c.contracts)
                loop(acc ++ contractsDefs.map(_.id), tail ++ contractsDefs)

              case _ => loop(acc, tail)
            }
        }
      }
      loop(Nil, definitions)
    }
  }

}
