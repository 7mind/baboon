package io.septimalmind.baboon.translator.swift

import io.septimalmind.baboon.translator.swift.SwTypes.*
import io.septimalmind.baboon.translator.swift.SwValue.{SwPackageId, SwType}
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.Quote

class SwTypeTranslator {
  def asSwRef(tpe: TypeRef, domain: Domain, evo: BaboonEvolution): TextTree[SwValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        q"${asSwType(id, domain, evo)}"

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
    }
  }

  def asSwType(tpe: TypeId, domain: Domain, evo: BaboonEvolution): SwType = {
    tpe match {
      case b: TypeId.BuiltinScalar =>
        b match {
          case TypeId.Builtins.bit  => swBool
          case TypeId.Builtins.i08  => swInt8
          case TypeId.Builtins.i16  => swInt16
          case TypeId.Builtins.i32  => swInt32
          case TypeId.Builtins.i64  => swInt64
          case TypeId.Builtins.u08  => swUInt8
          case TypeId.Builtins.u16  => swUInt16
          case TypeId.Builtins.u32  => swUInt32
          case TypeId.Builtins.u64  => swUInt64
          case TypeId.Builtins.f32  => swFloat
          case TypeId.Builtins.f64  => swDouble
          case TypeId.Builtins.f128 => baboonDecimal
          case TypeId.Builtins.str  => swString
          case TypeId.Builtins.bytes => swData
          case TypeId.Builtins.uid  => swUUID
          case TypeId.Builtins.tsu  => swDate
          case TypeId.Builtins.tso  => baboonDateTimeOffset
          case other => throw new IllegalArgumentException(s"Unexpected: $other")
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
    toSwPkg(
      p,
      version,
      omitVersion = version == evolution.latest,
    )
  }

  def toSwPkg(p: Pkg, version: Version, omitVersion: Boolean): SwPackageId = {
    val verString = "v" + version.v.toString
      .split('.')
      .mkString("_")

    val base = p.path.map(_.toLowerCase)
    val segments = if (omitVersion) {
      base
    } else {
      base :+ verString
    }

    SwPackageId(segments)
  }

  private def asSwTypeDerefForeigns(tid: TypeId.User, domain: Domain, evolution: BaboonEvolution): SwType = {
    domain.defs.meta.nodes(tid) match {
      case DomainMember.User(_, defn: Typedef.Foreign, _, _) =>
        val fe    = defn.bindings("swift")
        val parts = fe.decl.split('.').toList
        assert(parts.length > 1)
        val pkg = parts.init
        val id  = parts.last
        SwType(SwPackageId(NEList.unsafeFrom(pkg)), id)
      case _ =>
        toSwTypeRefKeepForeigns(tid, domain, evolution)
    }
  }

  def toSwTypeRefKeepForeigns(tid: TypeId.User, domain: Domain, evolution: BaboonEvolution): SwType = {
    val version = domain.version
    val pkg     = toSwPkg(tid.pkg, version, evolution)
    val typeName = renderScopedTypeName(tid, version, evolution)

    tid.owner match {
      case Owner.Adt(id) =>
        val parentOwnerParts = renderOwner(id.owner)
        val parentPkg        = SwPackageId(pkg.parts ++ parentOwnerParts)
        val ownerName        = renderTypeName(id.name.name, version, evolution)
        SwType(parentPkg, typeName, importAs = Some(toSnakeCase(ownerName)))
      case other =>
        val ownerAsPrefix = renderOwner(other)
        SwType(SwPackageId(pkg.parts ++ ownerAsPrefix), typeName)
    }
  }

  def fixtureClassName(tid: TypeId.User, domain: Domain, evolution: BaboonEvolution): String = {
    val swType = toSwTypeRefKeepForeigns(tid, domain, evolution)
    s"${swType.name}_Fixture"
  }

  private def renderScopedTypeName(tid: TypeId.User, version: Version, evolution: BaboonEvolution): String = {
    val baseName    = renderTypeName(tid.name.name, version, evolution)
    val ownerPrefix = renderOwnerTypePrefix(tid.owner, version, evolution)
    if (ownerPrefix.isEmpty) {
      baseName
    } else {
      s"${ownerPrefix.mkString("_")}_$baseName"
    }
  }

  private def renderTypeName(baseName: String, version: Version, evolution: BaboonEvolution): String = {
    if (version == evolution.latest) {
      baseName
    } else {
      val normalizedVersion = version.v.toString.map {
        case c if c.isLetterOrDigit => c
        case _                      => '_'
      }
      s"${baseName}_v_$normalizedVersion"
    }
  }

  private def renderOwner(owner: Owner): Seq[String] = {
    owner match {
      case Owner.Toplevel => Seq.empty
      case Owner.Ns(path) => path.map(_.name.toLowerCase)
      case Owner.Adt(id)  => renderOwner(id.owner) :+ id.name.name.toLowerCase
    }
  }

  private def renderOwnerTypePrefix(owner: Owner, version: Version, evolution: BaboonEvolution): Seq[String] = {
    owner match {
      case Owner.Toplevel => Seq.empty
      case Owner.Ns(path) => path.map(p => toTypeNameSegment(p.name))
      case Owner.Adt(id) =>
        renderOwnerTypePrefix(id.owner, version, evolution) :+ toTypeNameSegment(renderTypeName(id.name.name, version, evolution))
    }
  }

  private def toTypeNameSegment(raw: String): String = {
    val normalized = raw.map {
      case c if c.isLetterOrDigit => c
      case _                      => '_'
    }
    val prefixed = if (normalized.nonEmpty && normalized.head.isDigit) {
      s"T_$normalized"
    } else {
      normalized
    }
    val parts = prefixed.split('_').toList.filter(_.nonEmpty)
    if (parts.isEmpty) {
      "T"
    } else {
      parts.map(part => part.head.toUpper + part.tail).mkString("_")
    }
  }

  def effectiveSwPkg(owner: Owner, domain: Domain, evo: BaboonEvolution): SwPackageId = {
    val basePkg = toSwPkg(domain.id, domain.version, evo)
    owner match {
      case Owner.Toplevel  => basePkg
      case Owner.Ns(path)  => SwPackageId(NEList.unsafeFrom(basePkg.parts.toList ++ path.map(_.name.toLowerCase)))
      case Owner.Adt(id)   => effectiveSwPkg(id.owner, domain, evo)
    }
  }

  def toSnakeCase(name: String): String = {
    name.replaceAll("([A-Z]+)([A-Z][a-z])", "$1_$2")
      .replaceAll("([a-z\\d])([A-Z])", "$1_$2")
      .toLowerCase
  }

  private val swiftKeywords: Set[String] = Set(
    "associatedtype", "class", "deinit", "enum", "extension", "fileprivate",
    "func", "import", "init", "inout", "internal", "let", "open", "operator",
    "private", "protocol", "public", "rethrows", "static", "struct",
    "subscript", "typealias", "var", "break", "case", "continue", "default",
    "defer", "do", "else", "fallthrough", "for", "guard", "if", "in",
    "repeat", "return", "switch", "where", "while", "as", "catch", "false",
    "is", "nil", "self", "Self", "super", "throw", "throws", "true", "try",
    "async", "await", "Any", "Protocol", "Type",
  )

  def escapeSwiftKeyword(name: String): String = {
    if (swiftKeywords.contains(name)) s"`$name`" else name
  }
}
