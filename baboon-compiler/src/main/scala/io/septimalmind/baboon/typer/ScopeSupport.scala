package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.model.*
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.parser.model.issues.TyperIssue
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.Scope.{LeafScope, NestedScope, RootScope, ScopeName, SubScope}
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.NEList

import scala.annotation.tailrec

trait ScopeSupport[F[+_, +_]] {
  def resolveScopedRef(
    name: ScopedRef,
    scope: Scope[ExtendedRawDefn],
    pkg: Pkg,
    meta: RawNodeMeta,
  ): F[NEList[BaboonIssue], TypeId.User]

  def resolveTypeId(
    prefix: List[RawTypeName],
    name: RawTypeName,
    scope: Scope[ExtendedRawDefn],
    pkg: Pkg,
    meta: RawNodeMeta,
  ): F[NEList[BaboonIssue], TypeId]

  def resolveUserTypeId(
    name: RawTypeName,
    scope: Scope[ExtendedRawDefn],
    pkg: Pkg,
    meta: RawNodeMeta,
  ): F[NEList[BaboonIssue], TypeId.User]

  /** The scope that `path` denotes when looked up from `scope`, if it denotes one: the first name is
    * searched for in `scope` and the scopes around it, every further name is a member of the one before.
    */
  def resolveScope(
    path: NEList[RawTypeName],
    scope: Scope[ExtendedRawDefn],
  ): Option[NestedScope[ExtendedRawDefn]]

  /** The alias that `prefix.name` denotes when looked up from `scope`, if it denotes one. */
  def resolveAlias(
    prefix: List[RawTypeName],
    name: RawTypeName,
    scope: Scope[ExtendedRawDefn],
  ): Option[ScopeSupport.ResolvedAlias]

  /** The scope that the first name of a reference with the lookup scope `from` is looked up from,
    * for a reference standing in the body of the definition `enclosing`.
    */
  def lookupScope(
    from: RawLookupScope,
    enclosing: Scope[ExtendedRawDefn],
  ): Scope[ExtendedRawDefn]

  def ownerOf(
    scope: NestedScope[ExtendedRawDefn],
    pkg: Pkg,
  ): F[NEList[BaboonIssue], Owner]
}

object ScopeSupport {
  case class LookupResult(suffix: List[Scope[ExtendedRawDefn]], scope: NestedScope[ExtendedRawDefn])

  /** @param target the aliased type as written in the alias
    * @param scope  the alias itself: `target` stands in its body, so that is where its names are looked up from
    */
  case class ResolvedAlias(target: RawTypeRef, scope: NestedScope[ExtendedRawDefn])

  class ScopeSupportImpl[F[+_, +_]: Error2](
    types: TypeInfo
  ) extends ScopeSupport[F] {

    def resolveScopedRef(
      name: ScopedRef,
      scope: Scope[ExtendedRawDefn],
      pkg: Pkg,
      meta: RawNodeMeta,
    ): F[NEList[BaboonIssue], TypeId.User] = {
      val found = findScope(NEList(ScopeName(name.path.head.name)), lookupScope(name.scope, scope))
      found match {
        case Some(found) =>
          for {
            scopeOfFound <- lookupName(name.path.tail, found, List.empty, meta)
            fullPath      = List(found) ++ scopeOfFound.suffix
            resolved <- resolveUserTypeId(
              name.path.last,
              fullPath.last,
              pkg,
              meta,
            )
          } yield {
            resolved
          }

        case None =>
          F.fail(BaboonIssue.of(TyperIssue.NameNotFound(pkg, name, meta)))
      }
    }

    @tailrec
    private def lookupName(
      names: Seq[RawTypeName],
      in: Scope[ExtendedRawDefn],
      suffix: List[Scope[ExtendedRawDefn]],
      meta: RawNodeMeta,
    ): F[NEList[BaboonIssue], LookupResult] = {
      names.headOption match {
        case Some(value) =>
          in match {
            case s: SubScope[ExtendedRawDefn] =>
              s.nested.toMap.get(ScopeName(value.name)) match {
                case Some(value) =>
                  lookupName(names.tail, value, suffix :+ value, meta)
                case None =>
                  F.fail(BaboonIssue.of(TyperIssue.NamSeqeNotFound(names, s, meta)))
              }
            case _ =>
              F.fail(BaboonIssue.of(TyperIssue.UnexpectedScoping(List(in), meta)))
          }
        case None =>
          in match {
            // Resolved scope may be a Leaf (concrete DTO / contract / enum / id / foreign /
            // alias) OR a SubScope (a `RawAdt` or namespace). PR-63 (M20.2) widens the
            // resolution surface: ADT inheritance refs like `+ ErrorAtom` legitimately point at
            // the ADT's SubScope, and the dependency-extraction path in `BaboonTyper.deps`
            // routes them through here. Per Q-FU-3, resolution succeeds for any NestedScope;
            // downstream callers that only accept a leaf shape (e.g. `dtoParentToDefs`) check
            // the resolved id's defn kind explicitly.
            case s: NestedScope[ExtendedRawDefn] =>
              F.pure(LookupResult(suffix, s))
            case b =>
              F.fail(BaboonIssue.of(TyperIssue.UnexpectedScopeLookup(b, meta)))
          }
      }

    }

    def resolveUserTypeId(
      name: RawTypeName,
      scope: Scope[ExtendedRawDefn],
      pkg: Pkg,
      meta: RawNodeMeta,
    ): F[NEList[BaboonIssue], TypeId.User] = {
      for {
        id <- resolveTypeId(List.empty, name, scope, pkg, meta)
        userId <- id match {
          case id: TypeId.Builtin =>
            F.fail(BaboonIssue.of(TyperIssue.UnexpectedBuiltin(id, pkg, meta)))
          case u: TypeId.User =>
            F.pure(u)
        }
      } yield {
        userId
      }
    }

    def convertTypename(
      raw: RawTypeName,
      meta: RawNodeMeta,
    ): F[NEList[BaboonIssue], TypeName] = {
      for {
        typename <- F.pure(TypeName(raw.name))
        _        <- SymbolNames.validTypeName(typename, meta)
      } yield {
        typename
      }
    }

    def resolveTypeId(
      prefix: List[RawTypeName],
      name: RawTypeName,
      scope: Scope[ExtendedRawDefn],
      pkg: Pkg,
      meta: RawNodeMeta,
    ): F[NEList[BaboonIssue], TypeId] = {
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
            F.fromOption(BaboonIssue.of(TyperIssue.UnexpectedNonBuiltin(typename, pkg, scope, meta))) {
              asBuiltin(typename)
            }
        }
      } yield {
        result
      }
    }

    private def pathToOwner(defnPath: List[Scope[ExtendedRawDefn]], pkg: Pkg): F[NEList[BaboonIssue], Owner] = {

      for {
        ns <- namespaceOf(defnPath)
        nsOwner =
          if (ns.isEmpty) {
            F.pure(Owner.Toplevel)
          } else {
            F.pure(Owner.Ns(ns))
          }
        out <- defnPath.lastOption match {
          case Some(value: SubScope[ExtendedRawDefn]) =>
            value.defn.defn match {
              case a: RawAdt =>
                for {
                  name      <- convertTypename(a.name, a.meta)
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
            F.pure(Owner.Toplevel)
        }
      } yield {
        out
      }
    }

    private def namespaceOf(
      path: List[Scope[ExtendedRawDefn]]
    ): F[NEList[BaboonIssue], List[TypeName]] = {
      path match {
        case head :: tail =>
          for {
            next <- namespaceOf(tail)
            out <- head match {
              case _: RootScope[_] =>
                F.pure(next)

              case scope: Scope.SubScope[_] =>
                assert(
                  scope.defn.defn.isInstanceOf[RawAdt] ||
                  scope.defn.defn.isInstanceOf[RawNamespace] ||
                  scope.defn.defn.isInstanceOf[RawService]
                )
                F.pure(List(TypeName(scope.name.name)) ++ next)
              case scope: Scope.LeafScope[_] =>
                assert(
                  !scope.defn.defn.isInstanceOf[RawAdt] &&
                  !scope.defn.defn.isInstanceOf[RawNamespace] &&
                  !scope.defn.defn.isInstanceOf[RawService]
                )
                assert(tail.isEmpty, "typedefs must terminate type path")
                F.pure(List.empty)
            }
          } yield {
            out
          }

        case Nil =>
          F.pure(List.empty)
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

    def resolveScope(path: NEList[RawTypeName], scope: Scope[ExtendedRawDefn]): Option[NestedScope[ExtendedRawDefn]] = {
      findScope(path.map(n => ScopeName(n.name)), scope)
    }

    /** The scope that the path `needles` denotes when looked up from `scope`. Only the first name is
      * searched for in the enclosing scopes; every further name must be a member of the scope before it.
      */
    private def findScope(needles: NEList[ScopeName], scope: Scope[ExtendedRawDefn]): Option[NestedScope[ExtendedRawDefn]] = {
      findVisible(needles.head, scope).flatMap(memberAt(needles.tail, _))
    }

    /** The nearest scope called `name` that is visible from `scope`: `scope` itself, one of its
      * members, or the same of an enclosing scope.
      */
    private def findVisible(name: ScopeName, scope: Scope[ExtendedRawDefn]): Option[NestedScope[ExtendedRawDefn]] = {
      scope match {
        case s: RootScope[ExtendedRawDefn] =>
          s.nested.get(name)

        case s: LeafScope[ExtendedRawDefn] =>
          Some(s)
            .filter(_.name == name)
            .orElse {
              s.defn.declaringAdtOf(s) match {
                case Some(declaringAdt) => findFromReEmittedBranch(name, s, declaringAdt)
                case None               => findVisible(name, s.defn.parentOf(s))
              }
            }

        case s: SubScope[ExtendedRawDefn] =>
          Some(s)
            .filter(_.name == name)
            .orElse(
              s.nested.toMap
                .get(name)
            )
            .orElse(findVisible(name, s.defn.parentOf(s)))
      }
    }

    @tailrec
    private def memberAt(path: List[ScopeName], scope: NestedScope[ExtendedRawDefn]): Option[NestedScope[ExtendedRawDefn]] = {
      path match {
        case Nil =>
          Some(scope)
        case name :: rest =>
          scope match {
            case s: SubScope[ExtendedRawDefn] =>
              s.nested.toMap.get(name) match {
                case Some(member) => memberAt(rest, member)
                case None         => None
              }
            case _: LeafScope[ExtendedRawDefn] =>
              None
          }
      }
    }

    def lookupScope(from: RawLookupScope, enclosing: Scope[ExtendedRawDefn]): Scope[ExtendedRawDefn] = {
      from match {
        case RawLookupScope.Enclosing       => enclosing
        case RawLookupScope.Namespace(path) => namespaceAt(path.map(n => ScopeName(n.name)), rootOf(enclosing))
      }
    }

    @tailrec
    private def rootOf(scope: Scope[ExtendedRawDefn]): RootScope[ExtendedRawDefn] = {
      scope match {
        case root: RootScope[ExtendedRawDefn]     => root
        case nested: NestedScope[ExtendedRawDefn] => rootOf(nested.defn.parentOf(nested))
      }
    }

    /** The namespace reached from `scope` along `path`, or the deepest one on that path which has a
      * scope. A namespace declaring nothing but templates has none (`TemplateRegistryBuilder` removes
      * it together with them), and the names visible from it are those of the namespace around it.
      */
    @tailrec
    private def namespaceAt(path: List[ScopeName], scope: Scope[ExtendedRawDefn]): Scope[ExtendedRawDefn] = {
      path match {
        case Nil =>
          scope
        case name :: rest =>
          val members = scope match {
            case s: RootScope[ExtendedRawDefn] => s.nested
            case s: SubScope[ExtendedRawDefn]  => s.nested.toMap
            case _: LeafScope[ExtendedRawDefn] => Map.empty[ScopeName, NestedScope[ExtendedRawDefn]]
          }
          members.get(name) match {
            case Some(namespace: SubScope[ExtendedRawDefn]) if namespace.defn.defn.isInstanceOf[RawNamespace] =>
              namespaceAt(rest, namespace)
            case _ =>
              scope
          }
      }
    }

    /** Looks `name` up from the body of `branch`, an ADT branch that `+ X` re-emitted into another ADT.
      *
      * The name means what it means where the branch is declared: the receiving ADT may sit in a scope
      * where the name is not visible, or where it denotes a different type. The one exception is a
      * sibling branch of the declaring ADT, which is taken from the receiving ADT when that has a branch
      * of the same name, so branches re-emitted together (or overridden by the receiving ADT) keep
      * referring to each other.
      */
    private def findFromReEmittedBranch(
      name: ScopeName,
      branch: LeafScope[ExtendedRawDefn],
      declaringAdt: Scope[ExtendedRawDefn],
    ): Option[NestedScope[ExtendedRawDefn]] = {
      findVisible(name, declaringAdt).map {
        declared =>
          val isSiblingBranch = declared.defn.parentOf(declared).id == declaringAdt.id
          val counterpart = branch.defn.parentOf(branch) match {
            case receivingAdt: SubScope[ExtendedRawDefn] if isSiblingBranch => receivingAdt.nested.toMap.get(name)
            case _                                                          => None
          }
          counterpart.getOrElse(declared)
      }
    }

    def ownerOf(
      scope: NestedScope[ExtendedRawDefn],
      pkg: Pkg,
    ): F[NEList[BaboonIssue], Owner] = {
      pathToOwner(asPath(scope), pkg)
    }

    def resolveAlias(
      prefix: List[RawTypeName],
      name: RawTypeName,
      scope: Scope[ExtendedRawDefn],
    ): Option[ResolvedAlias] = {
      val needle = prefix.map(_.name).map(ScopeName.apply) ++: NEList(ScopeName(name.name))
      findScope(needle, scope).flatMap {
        found =>
          found.defn.defn match {
            case alias: RawAlias => Some(ResolvedAlias(alias.target, found))
            case _               => None
          }
      }
    }

    private def asBuiltin(name: TypeName): Option[TypeId.Builtin] = {
      if (types.isBultinCollection(TypeId.BuiltinCollection(name))) {
        Some(TypeId.BuiltinCollection(name))
      } else if (types.isBultinScalar(TypeId.BuiltinScalar(name))) {
        Some(TypeId.BuiltinScalar(name))
      } else {
        None
      }
    }
  }
}
