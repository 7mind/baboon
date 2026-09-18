package io.septimalmind.baboon.translator.swift

import io.septimalmind.baboon.translator.swift.SwTypes.*
import io.septimalmind.baboon.translator.swift.SwValue.{SwPackageId, SwType}
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.Quote

class SwTypeTranslator {
  private def foreignAlias(defn: Typedef.Foreign): Option[TypeRef] = {
    def alias(entry: Typedef.ForeignEntry): Option[TypeRef] = entry.mapping match {
      case Typedef.ForeignMapping.BaboonRef(ref) => Some(ref)
      case _: Typedef.ForeignMapping.Custom      => None
    }
    defn.bindings.get(BaboonLang.Swift).flatMap(alias).orElse {
      defn.bindings.valuesIterator.flatMap(alias).take(1).toList.headOption
    }
  }

  def asSwRef(tpe: TypeRef, domain: Domain, evo: BaboonEvolution): TextTree[SwValue] = {
    def resolveForeignAlias(id: TypeId.User): Option[TypeRef] = {
      domain.defs.meta.nodes(id) match {
        case DomainMember.User(_, defn: Typedef.Foreign, _, _) => foreignAlias(defn)
        case _                                                 => None
      }
    }

    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case uid: TypeId.User if resolveForeignAlias(uid).nonEmpty =>
            asSwRef(resolveForeignAlias(uid).get, domain, evo)
          case _ =>
            q"${asSwType(id, domain, evo)}"
        }

      case TypeRef.Constructor(id, args) if id == TypeId.Builtins.opt =>
        val inner = asSwRef(args.head, domain, evo)
        q"$inner?"
      case TypeRef.Constructor(id, args) if id == TypeId.Builtins.lst =>
        val inner = asSwRef(args.head, domain, evo)
        q"[$inner]"
      case TypeRef.Constructor(id, args) if id == TypeId.Builtins.set =>
        val inner = asSwRef(args.head, domain, evo)
        q"Set<$inner>"
      case TypeRef.Constructor(id, args) if id == TypeId.Builtins.map =>
        val keyRef = asSwRef(args.head, domain, evo)
        val valRef = asSwRef(args(1), domain, evo)
        q"[$keyRef: $valRef]"
      case TypeRef.Constructor(id, args) =>
        val tpe   = asSwType(id, domain, evo)
        val targs = args.map(asSwRef(_, domain, evo))
        q"$tpe<${targs.toSeq.join(", ")}>"
      // `any`-typed fields surface as the bundled `AnyOpaque` Swift enum (PR 9.1 runtime). The
      // `variant` / `underlying` distinction is encoded in the wire `meta` byte at codec time, not
      // in the Swift surface type — every `any` field is a `AnyOpaque` regardless of variant.
      case _: TypeRef.Any => q"$baboonAnyOpaque"
    }
  }

  def asSwType(tpe: TypeId, domain: Domain, evo: BaboonEvolution): SwType = {
    tpe match {
      case b: TypeId.BuiltinScalar =>
        b match {
          case TypeId.Builtins.bit   => swBool
          case TypeId.Builtins.i08   => swInt8
          case TypeId.Builtins.i16   => swInt16
          case TypeId.Builtins.i32   => swInt32
          case TypeId.Builtins.i64   => swInt64
          case TypeId.Builtins.u08   => swUInt8
          case TypeId.Builtins.u16   => swUInt16
          case TypeId.Builtins.u32   => swUInt32
          case TypeId.Builtins.u64   => swUInt64
          case TypeId.Builtins.f32   => swFloat
          case TypeId.Builtins.f64   => swDouble
          case TypeId.Builtins.f128  => baboonDecimal
          case TypeId.Builtins.str   => swString
          case TypeId.Builtins.bytes => swData
          case TypeId.Builtins.uid   => swUUID
          case TypeId.Builtins.tsu   => swDate
          case TypeId.Builtins.tso   => baboonDateTimeOffset
          case other                 => throw new IllegalArgumentException(s"Unexpected: $other")
        }
      case TypeId.Builtins.map => swDictionary
      case TypeId.Builtins.lst => swArray
      case TypeId.Builtins.set => swSet
      case TypeId.Builtins.opt =>
        throw new IllegalArgumentException("opt should be handled via nullable T?, not as a raw type constructor")
      case uid: TypeId.User => asSwTypeDerefForeigns(uid, domain, evo)
      case other            => throw new IllegalArgumentException(s"Unexpected: $other")
    }
  }

  def toSwPkg(p: Pkg, version: Version, evolution: BaboonEvolution): SwPackageId = {
    SwPackageId(domainModuleName(p, version, evolution))
  }

  private def asSwTypeDerefForeigns(tid: TypeId.User, domain: Domain, evolution: BaboonEvolution): SwType = {

    domain.defs.meta.nodes(tid) match {
      case DomainMember.User(_, defn: Typedef.Foreign, _, _) =>
        def aliasType(ref: TypeRef): SwType = ref match {
          case TypeRef.Scalar(refId) => asSwType(refId, domain, evolution)
          case _                     => toSwTypeRefKeepForeigns(tid, domain, evolution)
        }

        defn.bindings.get(BaboonLang.Swift).map(_.mapping) match {
          case Some(Typedef.ForeignMapping.BaboonRef(ref)) => aliasType(ref)
          case Some(Typedef.ForeignMapping.Custom(decl, _)) =>
            val parts = decl.split('.').toList
            assert(parts.length > 1)
            SwType(SwPackageId(NEList.unsafeFrom(parts.init)), parts.last)
          case None =>
            foreignAlias(defn) match {
              case Some(ref) => aliasType(ref)
              case None      => throw new IllegalStateException(s"Missing swift binding for foreign type: ${defn.id}")
            }
        }
      case _ =>
        toSwTypeRefKeepForeigns(tid, domain, evolution)
    }
  }

  def toSwTypeRefKeepForeigns(tid: TypeId.User, domain: Domain, evolution: BaboonEvolution): SwType = {
    val version                    = domain.version
    val pkg                        = toSwPkg(tid.pkg, version, evolution)
    val (qualifiedName, localName) = renderScopedTypeName(tid)

    tid.owner match {
      case Owner.Adt(id) =>
        val parentOwnerParts = renderOwner(id.owner)
        val parentPkg        = SwPackageId(pkg.parts ++ parentOwnerParts)
        val ownerName        = id.name.name
        SwType(parentPkg, qualifiedName, importAs = Some(toSnakeCase(ownerName)), localName = Some(localName))
      case other =>
        val ownerAsPrefix = renderOwner(other)
        SwType(SwPackageId(pkg.parts ++ ownerAsPrefix), qualifiedName, localName = Some(localName))
    }
  }

  def fixtureClassName(tid: TypeId.User, domain: Domain, evolution: BaboonEvolution): String = {
    val swType = toSwTypeRefKeepForeigns(tid, domain, evolution)
    s"${swType.name.replace('.', '_')}_Fixture"
  }

  def domainModuleName(pkg: Pkg, version: Version, evo: BaboonEvolution): String = {
    val base = pkg.path.map(s => s.head.toUpper + s.tail).mkString("")
    if (version == evo.latest) base
    else base + "_v" + version.v.toString.replace('.', '_')
  }

  private def renderScopedTypeName(tid: TypeId.User): (String, String) = {
    val baseName  = tid.name.name
    val ownerPath = renderOwnerQualifiedPath(tid.owner)
    val qualified =
      if (ownerPath.isEmpty) baseName
      else s"${ownerPath.mkString(".")}.$baseName"
    (qualified, baseName)
  }

  private def renderOwnerQualifiedPath(owner: Owner): Seq[String] = {
    owner match {
      case Owner.Toplevel => Seq.empty
      case Owner.Ns(path) => path.map(_.name.toLowerCase)
      case Owner.Adt(id)  => renderOwnerQualifiedPath(id.owner) :+ id.name.name
    }
  }

  private def renderOwner(owner: Owner): Seq[String] = {
    owner match {
      case Owner.Toplevel => Seq.empty
      case Owner.Ns(path) => path.map(_.name.toLowerCase)
      case Owner.Adt(id)  => renderOwner(id.owner) :+ id.name.name.toLowerCase
    }
  }

  def effectiveSwPkg(owner: Owner, domain: Domain, evo: BaboonEvolution): SwPackageId = {
    val basePkg = toSwPkg(domain.id, domain.version, evo)
    owner match {
      case Owner.Toplevel => basePkg
      case Owner.Ns(path) => SwPackageId(NEList.unsafeFrom(basePkg.parts.toList ++ path.map(_.name.toLowerCase)))
      case Owner.Adt(id)  => effectiveSwPkg(id.owner, domain, evo)
    }
  }

  def toSnakeCase(name: String): String = {
    name
      .replaceAll("([A-Z]+)([A-Z][a-z])", "$1_$2")
      .replaceAll("([a-z\\d])([A-Z])", "$1_$2")
      .toLowerCase
  }

  private val swiftKeywords: Set[String] = Set(
    "associatedtype",
    "class",
    "deinit",
    "enum",
    "extension",
    "fileprivate",
    "func",
    "import",
    "init",
    "inout",
    "internal",
    "let",
    "open",
    "operator",
    "private",
    "protocol",
    "public",
    "rethrows",
    "static",
    "struct",
    "subscript",
    "typealias",
    "var",
    "break",
    "case",
    "continue",
    "default",
    "defer",
    "do",
    "else",
    "fallthrough",
    "for",
    "guard",
    "if",
    "in",
    "repeat",
    "return",
    "switch",
    "where",
    "while",
    "as",
    "catch",
    "false",
    "is",
    "nil",
    "self",
    "Self",
    "super",
    "throw",
    "throws",
    "true",
    "try",
    "async",
    "await",
    "Any",
    "Protocol",
    "Type",
  )

  def escapeSwiftKeyword(name: String): String = {
    if (swiftKeywords.contains(name)) s"`$name`" else name
  }
}
