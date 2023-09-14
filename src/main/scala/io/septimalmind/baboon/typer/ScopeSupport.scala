package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.parser.model.{RawAdt, RawTypeName, ScopedRef}
import io.septimalmind.baboon.typer.BaboonTyper.FullRawDefn
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.Scope.{LeafScope, RootScope, ScopeName, SubScope}
import izumi.functional.IzEither.*
import izumi.fundamentals.collections.nonempty.NEList

trait ScopeSupport {
  def resolveScopedRef(
    name: ScopedRef,
    path: NEList[Scope[FullRawDefn]],
    pkg: Pkg
  ): Either[NEList[BaboonIssue.TyperIssue], TypeId.User]

  def resolveTypeId(name: RawTypeName,
                    path: NEList[Scope[FullRawDefn]],
                    pkg: Pkg,
  ): Either[NEList[BaboonIssue.TyperIssue], TypeId]

  def resolveUserTypeId(name: RawTypeName,
                        path: NEList[Scope[FullRawDefn]],
                        pkg: Pkg,
  ): Either[NEList[BaboonIssue.TyperIssue], TypeId.User]
}

object ScopeSupport {
  case class FoundDefn(defn: FullRawDefn,
                       path: List[Scope[FullRawDefn]],
                       scope: Scope[FullRawDefn])

  case class LookupResult(suffix: List[Scope[FullRawDefn]],
                          scope: LeafScope[FullRawDefn])

  class ScopeSupportImpl extends ScopeSupport {
    def resolveScopedRef(
      name: ScopedRef,
      path: NEList[Scope[FullRawDefn]],
      pkg: Pkg
    ): Either[NEList[BaboonIssue.TyperIssue], TypeId.User] = {

      findDefn(ScopeName(name.path.head.name), path.reverse.toList) match {
        case Some(found) =>
          for {
            scope <- lookupName(name.path.tail, found.scope, List.empty)
            fullPath = (found.path :+ found.scope) ++ scope.suffix
            resolved <- resolveUserTypeId(
              name.path.last,
              NEList.unsafeFrom(fullPath.init),
              pkg
            )
          } yield {
            resolved
          }

        case None =>
          Left(NEList(BaboonIssue.NameNotFound(pkg, name)))
      }
    }

    def lookupName(
      names: Seq[RawTypeName],
      in: Scope[FullRawDefn],
      suffix: List[Scope[FullRawDefn]]
    ): Either[NEList[BaboonIssue.TyperIssue], LookupResult] = {
      names.headOption match {
        case Some(value) =>
          in match {
            case s: SubScope[FullRawDefn] =>
              s.nested.toMap.get(ScopeName(value.name)) match {
                case Some(value) =>
                  lookupName(names.tail, value, suffix :+ value)
                case None =>
                  Left(NEList(BaboonIssue.NamSeqeNotFound(names)))
              }
            case _ =>
              Left(NEList(BaboonIssue.UnexpectedScoping(List(in))))
          }
        case None =>
          in match {
            case s: LeafScope[FullRawDefn] =>
              Right(LookupResult(suffix, s))
            case b =>
              Left(NEList(BaboonIssue.UnexpectedScopeLookup(b)))
          }
      }

    }

    def resolveUserTypeId(name: RawTypeName,
                          path: NEList[Scope[FullRawDefn]],
                          pkg: Pkg,
    ): Either[NEList[BaboonIssue.TyperIssue], TypeId.User] = {
      for {
        id <- resolveTypeId(name, path, pkg)
        userId <- id match {
          case id: TypeId.Builtin =>
            Left(NEList(BaboonIssue.UnexpectedBuiltin(id, pkg, path)))
          case u: TypeId.User =>
            Right(u)
        }
      } yield {
        userId
      }
    }

    def resolveTypeId(name: RawTypeName,
                      path: NEList[Scope[FullRawDefn]],
                      pkg: Pkg,
    ): Either[NEList[BaboonIssue.TyperIssue], TypeId] = {
      for {
        typename <- Right(TypeName(name.name))
        _ <- SymbolNames.validTypeName(typename)
        result <- (findDefn(ScopeName(name.name), path.reverse.toList) match {
          case Some(value) =>
            for {
              owner <- pathToOwner(value.path, pkg)
            } yield {
              TypeId.User(pkg, owner, typename)
            }

          case None =>
            asBuiltin(typename).toRight(
              NEList(BaboonIssue.UnexpectedNonBuiltin(typename, pkg, path))
            )
        })
      } yield {
        result
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

    private def findDefn(
      needle: ScopeName,
      reversePath: List[Scope[FullRawDefn]]
    ): Option[FoundDefn] = {
      reversePath.headOption match {
        case Some(s: RootScope[FullRawDefn]) =>
          s.nested
            .get(needle)
            .map(n => FoundDefn(n.defn, reversePath, n))

        case Some(s: LeafScope[FullRawDefn]) =>
          Some(s)
            .filter(_.name == needle)
            .map(n => FoundDefn(n.defn, reversePath.reverse, n))
            .orElse(findDefn(needle, reversePath.tail))

        case Some(s: SubScope[FullRawDefn]) =>
          s.nested.toMap
            .get(needle)
            .orElse(Some(s).filter(_.name == needle))
            .map(n => FoundDefn(n.defn, reversePath.reverse, n))
            .orElse(findDefn(needle, reversePath.tail))

        case None =>
          None
      }
    }

    private def pathToOwner(defnPath: List[Scope[FullRawDefn]],
                            pkg: Pkg,
    ): Either[NEList[BaboonIssue.TyperIssue], Owner] = {
      defnPath match {
        case (_: Scope.RootScope[FullRawDefn]) :: Nil => Right(Owner.Toplevel)
        case (r: Scope.RootScope[FullRawDefn]) :: (sub: Scope.SubScope[
              FullRawDefn
            ]) :: Nil =>
          sub.defn.defn match {
            case a: RawAdt =>
              for {
                id <- resolveUserTypeId(a.name, NEList(r), pkg)
              } yield {
                Owner.Adt(id)
              }
            case _ =>
              Left(NEList(BaboonIssue.UnexpectedScoping(defnPath)))
          }

        case Nil =>
          Right(Owner.Toplevel)
        case e =>
          Left(NEList(BaboonIssue.UnexpectedScoping(e)))
      }
    }
  }
}
