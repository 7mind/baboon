package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.model.*
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.Scope.{
  LeafScope,
  NestedScope,
  RootScope,
  ScopeName,
  SubScope
}
import izumi.fundamentals.collections.nonempty.NEList

import scala.annotation.tailrec

trait ScopeSupport {
  def resolveScopedRef(
    name: ScopedRef,
    scope: Scope[ExtendedRawDefn],
    pkg: Pkg,
    meta: RawNodeMeta
  ): Either[NEList[BaboonIssue.TyperIssue], TypeId.User]

  def resolveTypeId(
    prefix: List[RawTypeName],
    name: RawTypeName,
    scope: Scope[ExtendedRawDefn],
    pkg: Pkg,
    meta: RawNodeMeta
  ): Either[NEList[BaboonIssue.TyperIssue], TypeId]

  def resolveUserTypeId(
    name: RawTypeName,
    scope: Scope[ExtendedRawDefn],
    pkg: Pkg,
    meta: RawNodeMeta
  ): Either[NEList[BaboonIssue.TyperIssue], TypeId.User]
}

object ScopeSupport {
  case class LookupResult(suffix: List[Scope[ExtendedRawDefn]],
                          scope: LeafScope[ExtendedRawDefn])

  class ScopeSupportImpl extends ScopeSupport {
    def resolveScopedRef(
      name: ScopedRef,
      scope: Scope[ExtendedRawDefn],
      pkg: Pkg,
      meta: RawNodeMeta
    ): Either[NEList[BaboonIssue.TyperIssue], TypeId.User] = {
      val found = findScope(NEList(ScopeName(name.path.head.name)), scope)
      found match {
        case Some(found) =>
          for {
            scopeOfFound <- lookupName(name.path.tail, found, List.empty, meta)
            fullPath = List(found) ++ scopeOfFound.suffix
            resolved <- resolveUserTypeId(
              name.path.last,
              fullPath.last,
              pkg,
              meta
            )
          } yield {
            resolved
          }

        case None =>
          Left(NEList(BaboonIssue.NameNotFound(pkg, name, meta)))
      }
    }

    @tailrec
    private def lookupName(
      names: Seq[RawTypeName],
      in: Scope[ExtendedRawDefn],
      suffix: List[Scope[ExtendedRawDefn]],
      meta: RawNodeMeta
    ): Either[NEList[BaboonIssue.TyperIssue], LookupResult] = {
      names.headOption match {
        case Some(value) =>
          in match {
            case s: SubScope[ExtendedRawDefn] =>
              s.nested.toMap.get(ScopeName(value.name)) match {
                case Some(value) =>
                  lookupName(names.tail, value, suffix :+ value, meta)
                case None =>
                  Left(NEList(BaboonIssue.NamSeqeNotFound(names, s, meta)))
              }
            case _ =>
              Left(NEList(BaboonIssue.UnexpectedScoping(List(in), meta)))
          }
        case None =>
          in match {
            case s: LeafScope[ExtendedRawDefn] =>
              Right(LookupResult(suffix, s))
            case b =>
              Left(NEList(BaboonIssue.UnexpectedScopeLookup(b, meta)))
          }
      }

    }

    def resolveUserTypeId(
      name: RawTypeName,
      scope: Scope[ExtendedRawDefn],
      pkg: Pkg,
      meta: RawNodeMeta
    ): Either[NEList[BaboonIssue.TyperIssue], TypeId.User] = {
      for {
        id <- resolveTypeId(List.empty, name, scope, pkg, meta)
        userId <- id match {
          case id: TypeId.Builtin =>
            Left(NEList(BaboonIssue.UnexpectedBuiltin(id, pkg, meta)))
          case u: TypeId.User =>
            Right(u)
        }
      } yield {
        userId
      }
    }

    def convertTypename(
      raw: RawTypeName,
      meta: RawNodeMeta
    ): Either[NEList[BaboonIssue.TyperIssue], TypeName] = {
      for {
        typename <- Right(TypeName(raw.name))
        _ <- SymbolNames.validTypeName(typename, meta)
      } yield {
        typename
      }
    }

    def resolveTypeId(
      prefix: List[RawTypeName],
      name: RawTypeName,
      scope: Scope[ExtendedRawDefn],
      pkg: Pkg,
      meta: RawNodeMeta
    ): Either[NEList[BaboonIssue.TyperIssue], TypeId] = {
      for {
        typename <- convertTypename(name, meta)
        needle = prefix.map(_.name).map(ScopeName.apply) ++: NEList(
          ScopeName(name.name)
        )
        found = findScope(needle, scope)
        result <- found match {
          case Some(value) =>
            for {
              owner <- pathToOwner(asPath(value), pkg)
            } yield {
              val out = TypeId.User(pkg, owner, typename)
              out
            }
          case None =>
            asBuiltin(typename).toRight(
              NEList(
                BaboonIssue
                  .UnexpectedNonBuiltin(typename, pkg, scope, meta)
              )
            )
        }
      } yield {
        result
      }
    }

    private def pathToOwner(defnPath: List[Scope[ExtendedRawDefn]],
                            pkg: Pkg,
    ): Either[NEList[BaboonIssue.TyperIssue], Owner] = {

      for {
        ns <- namespaceOf(defnPath)
        nsOwner = if (ns.isEmpty) {
          Right(Owner.Toplevel)
        } else {
          Right(Owner.Ns(ns))
        }
        out <- defnPath.lastOption match {
          case Some(value: SubScope[ExtendedRawDefn]) =>
            value.defn.defn match {
              case a: RawAdt =>
                for {
                  name <- convertTypename(a.name, a.meta)
                  nextOwner <- pathToOwner(defnPath.init, pkg)
                } yield {
                  Owner.Adt(TypeId.User(pkg, nextOwner, name))
                }
              case _ =>
                assert(true)
                nsOwner
            }

          case Some(_) =>
            nsOwner
          case None =>
            assert(ns.isEmpty)
            Right(Owner.Toplevel)
        }
      } yield {
        out
      }
    }

    private def namespaceOf(
      path: List[Scope[ExtendedRawDefn]],
    ): Either[NEList[BaboonIssue.TyperIssue], List[TypeName]] = {
      path match {
        case head :: tail =>
          for {
            next <- namespaceOf(tail)
            out <- head match {
              case _: RootScope[_] =>
                Right(next)

              case scope: Scope.SubScope[_] =>
                assert(
                  scope.defn.defn.isInstanceOf[RawAdt] || scope.defn.defn
                    .isInstanceOf[RawNamespace]
                )
                Right(List(TypeName(scope.name.name)) ++ next)
              case scope: Scope.LeafScope[_] =>
                assert(
                  !scope.defn.defn.isInstanceOf[RawAdt] && !scope.defn.defn
                    .isInstanceOf[RawNamespace]
                )
                assert(tail.isEmpty, "typedefs must terminate type path")
                Right(List.empty)
            }
          } yield {
            out
          }

        case Nil =>
          Right(List.empty)
      }
    }

    private def asPath(
      scope: Scope[ExtendedRawDefn]
    ): List[Scope[ExtendedRawDefn]] = {

      def go(s: Scope[ExtendedRawDefn]): NEList[Scope[ExtendedRawDefn]] = {
        s match {
          case r: RootScope[ExtendedRawDefn] => NEList(r)
          case n: NestedScope[ExtendedRawDefn] =>
            go(n.defn.parentOf(n)) ++ NEList(n)
        }

      }
      go(scope).toList.init

    }

    private def findScope(needles: NEList[ScopeName],
                          scope: Scope[ExtendedRawDefn],
    ): Option[NestedScope[ExtendedRawDefn]] = {
      val head = needles.head

      val headScope = scope match {
        case s: RootScope[ExtendedRawDefn] =>
          s.nested.get(head)

        case s: LeafScope[ExtendedRawDefn] =>
          Some(s)
            .filter(_.name == head)
            .orElse(findScope(needles, s.defn.parentOf(s)))

        case s: SubScope[ExtendedRawDefn] =>
          Some(s)
            .filter(_.name == head)
            .orElse(
              s.nested.toMap
                .get(head)
            )
            .orElse(findScope(needles, s.defn.parentOf(s)))
      }

      NEList.from(needles.tail) match {
        case Some(value) =>
          headScope.flatMap(nested => findScope(value, nested))
        case None =>
          headScope
      }
    }

    private def asBuiltin(name: TypeName): Option[TypeId.Builtin] = {
      if (TypeId.Builtins.collections.map(_.name).contains(name)) {
        Some(TypeId.BuiltinCollection(name))
      } else if (TypeId.Builtins.scalars.map(_.name).contains(name)) {
        Some(TypeId.BuiltinScalar(name))
      } else {
        None
      }
    }
  }
}
