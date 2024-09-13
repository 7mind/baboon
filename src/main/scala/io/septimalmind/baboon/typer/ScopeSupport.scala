package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.parser.model.{
  RawAdt,
  RawContract,
  RawDto,
  RawEnum,
  RawForeign,
  RawNamespace,
  RawNodeMeta,
  RawTypeName,
  ScopedRef
}
import io.septimalmind.baboon.typer.BaboonTyper.FullRawDefn
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
import scala.collection.immutable.List

trait ScopeSupport {
  def resolveScopedRef(
    name: ScopedRef,
    path: NEList[Scope[FullRawDefn]],
    pkg: Pkg,
    meta: RawNodeMeta
  ): Either[NEList[BaboonIssue.TyperIssue], TypeId.User]

  def resolveTypeId(
    prefix: List[RawTypeName],
    name: RawTypeName,
    path: NEList[Scope[FullRawDefn]],
    pkg: Pkg,
    meta: RawNodeMeta
  ): Either[NEList[BaboonIssue.TyperIssue], TypeId]

  def resolveUserTypeId(
    name: RawTypeName,
    path: NEList[Scope[FullRawDefn]],
    pkg: Pkg,
    meta: RawNodeMeta
  ): Either[NEList[BaboonIssue.TyperIssue], TypeId.User]
}

object ScopeSupport {
  case class FoundDefn(path: List[Scope[FullRawDefn]],
                       scope: NestedScope[FullRawDefn]) {
    def defn: FullRawDefn = scope.defn
  }

  case class LookupResult(suffix: List[Scope[FullRawDefn]],
                          scope: LeafScope[FullRawDefn])

  class ScopeSupportImpl extends ScopeSupport {
    def resolveScopedRef(
      name: ScopedRef,
      path: NEList[Scope[FullRawDefn]],
      pkg: Pkg,
      meta: RawNodeMeta
    ): Either[NEList[BaboonIssue.TyperIssue], TypeId.User] = {
      val found = findDefn(ScopeName(name.path.head.name), path.reverse.toList)

      found match {
        case Some(found) =>
          for {
            scope <- lookupName(name.path.tail, found.scope, List.empty, meta)
            fullPath = (found.path :+ found.scope) ++ scope.suffix
            resolved <- resolveUserTypeId(
              name.path.last,
              NEList.unsafeFrom(fullPath.init),
              pkg,
              meta
            )
          } yield {
            resolved
          }

        case None =>
          System.exit(1)
          Left(NEList(BaboonIssue.NameNotFound(pkg, name, meta)))
      }
    }

    @tailrec
    private def lookupName(
      names: Seq[RawTypeName],
      in: Scope[FullRawDefn],
      suffix: List[Scope[FullRawDefn]],
      meta: RawNodeMeta
    ): Either[NEList[BaboonIssue.TyperIssue], LookupResult] = {
      names.headOption match {
        case Some(value) =>
          in match {
            case s: SubScope[FullRawDefn] =>
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
            case s: LeafScope[FullRawDefn] =>
              Right(LookupResult(suffix, s))
            case b =>
              Left(NEList(BaboonIssue.UnexpectedScopeLookup(b, meta)))
          }
      }

    }

    def resolveUserTypeId(
      name: RawTypeName,
      path: NEList[Scope[FullRawDefn]],
      pkg: Pkg,
      meta: RawNodeMeta
    ): Either[NEList[BaboonIssue.TyperIssue], TypeId.User] = {
      for {
        id <- resolveTypeId(List.empty, name, path, pkg, meta)
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
      path: NEList[Scope[FullRawDefn]],
      pkg: Pkg,
      meta: RawNodeMeta
    ): Either[NEList[BaboonIssue.TyperIssue], TypeId] = {
      for {
        typename <- convertTypename(name, meta)
        needle = prefix.map(_.name).map(ScopeName) ++ List(ScopeName(name.name))
//        _ <- Right(if (needle.toString().contains("TEST_SUB_A1")) {
//          println()
//          println(s"Looking for ${needle.map(_.name).mkString(">")}")
//        })
        found = findPrefixedDefn(needle, path.reverse.toList)
        result <- found match {
          case Some(value) =>
//            println(s"about to process $name")

            val fixedPath = value.path.last match {
              case scope: NestedScope[_] if scope.defn == value.defn =>
                value.path.init
              case _ => value.path
            }

            for {
              owner <- pathToOwner(fixedPath, pkg)
            } yield {
              val out = TypeId.User(pkg, owner, typename)

//              println(
//                s"$name => $out (prefix ${prefix.map(_.name).mkString(".")}; ${formatPath(
//                  path.reverse.toList
//                )} => ${formatPath(value.path)} => ${formatPath(fixedPath)})"
//              )
//              println()
              out
            }
          case None =>
            asBuiltin(typename).toRight(
              NEList(
                BaboonIssue.UnexpectedNonBuiltin(typename, pkg, path, meta)
              )
            )
        }
      } yield {
//        if (result.toString == "testpkg.pkg0/sub#TEST_SUB_A1") {
//          ???
//        }

        result
      }
    }

    def formatPath(path: List[Scope[FullRawDefn]]) = {
      path
        .map(_ match {
          case RootScope(pkg, nested) => "root"
          case scope: Scope.NestedScope[_] =>
            scope match {
              case SubScope(name, defn, nested) => s"sub:$name"
              case LeafScope(name, defn)        => s"leaf:$name"
            }
        })
        .mkString("<<")
    }

    private def findPrefixedDefn(
      needles: List[ScopeName],
      reversePath: List[Scope[FullRawDefn]]
    ): Option[FoundDefn] = {

      def subfind(needle: ScopeName,
                  reversePath: List[Scope[FullRawDefn]]): Option[FoundDefn] = {
        findDefn(needle, reversePath).orElse {
          reversePath match {
            case head :: tail =>
              subfind(needle, tail)
            case Nil =>
              None
          }

        }
      }

      val out = needles match {
        case Nil =>
          None
        case head :: Nil =>
          subfind(head, reversePath)
        case head :: tail =>
          subfind(head, reversePath).flatMap { found =>
            val newpath = if (found.path.last == found.scope) {
              found.path.reverse
            } else {
              (found.path ++ List(found.scope)).reverse
            }

//            if (needles.toString().contains("TEST_SUB_A1")) {
//
//              println(
//                s"Searching for ${needles.map(_.name).mkString(">")} in ${formatPath(reversePath)}"
//              )
//              println(
//                s"head found: ${found.defn.defn.name}, new path = ${formatPath(newpath)}"
//              )
//            }
            findPrefixedDefn(tail, newpath)
          }
      }
//      if (needles.toString().contains("TEST_SUB_A1")) {
//        println(
//          s"Searching for ${needles.map(_.name).mkString(">")} in ${formatPath(reversePath)} ==> ${out
//            .map(_.defn.defn.name)} @ ${out.map(d => formatPath(d.path))}"
//        )
//      }
      out
    }

    private def pathToOwner(defnPath: List[Scope[FullRawDefn]],
                            pkg: Pkg,
    ): Either[NEList[BaboonIssue.TyperIssue], Owner] = {

      for {
        ns <- namespaceOf(defnPath)
        nsOwner = if (ns.isEmpty) {
          Right(Owner.Toplevel)
        } else {
          Right(Owner.Ns(ns))
        }
//        _ <- Right(println(s"ns == ${ns.mkString(".")} => $nsOwner"))
        out <- defnPath.lastOption match {
          case Some(value: SubScope[FullRawDefn]) =>
            value.defn.defn match {
              case a: RawAdt =>
                for {
                  name <- convertTypename(a.name, a.meta)
                  nextOwner <- pathToOwner(defnPath.init, pkg)
                } yield {
//                  println(s"nextowner == ${nextOwner}")

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
      path: List[Scope[FullRawDefn]],
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

    private def findDefn(
      needle: ScopeName,
      reversePath: List[Scope[FullRawDefn]]
    ): Option[FoundDefn] = {
      reversePath.headOption match {
        case Some(s: RootScope[FullRawDefn]) =>
          s.nested
            .get(needle)
            .map(n => FoundDefn(reversePath, n))

        case Some(s: LeafScope[FullRawDefn]) =>
          Some(s)
            .filter(_.name == needle)
            .map(n => FoundDefn(reversePath.reverse, n))
            .orElse(findDefn(needle, reversePath.tail))

        case Some(s: SubScope[FullRawDefn]) =>
          s.nested.toMap
            .get(needle)
            .orElse(Some(s).filter(_.name == needle))
            .map(n => FoundDefn(reversePath.reverse, n))
            .orElse(findDefn(needle, reversePath.tail))

        case None =>
          None
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
