package io.septimalmind.baboon.translator.dart

import io.septimalmind.baboon.translator.dart.DtTypes.*
import io.septimalmind.baboon.translator.dart.DtValue.{DtPackageId, DtType}
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.Quote

class DtTypeTranslator {
  def asDtRef(tpe: TypeRef, domain: Domain, evo: BaboonEvolution): TextTree[DtValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        q"${asDtType(id, domain, evo)}"

      case TypeRef.Constructor(id, args) if id == TypeId.Builtins.opt =>
        val inner = asDtRef(args.head, domain, evo)
        q"$inner?"
      case TypeRef.Constructor(id, args) =>
        val tpe   = asDtType(id, domain, evo)
        val targs = args.map(asDtRef(_, domain, evo))
        q"$tpe<${targs.toSeq.join(", ")}>"
    }
  }

  def asDtType(tpe: TypeId, domain: Domain, evo: BaboonEvolution): DtType = {
    tpe match {
      case b: TypeId.BuiltinScalar =>
        b match {
          case TypeId.Builtins.bit                         => dtBool
          case TypeId.Builtins.i08 | TypeId.Builtins.i16 |
               TypeId.Builtins.i32 | TypeId.Builtins.i64 |
               TypeId.Builtins.u08 | TypeId.Builtins.u16 |
               TypeId.Builtins.u32 | TypeId.Builtins.u64  => dtInt
          case TypeId.Builtins.f32 | TypeId.Builtins.f64   => dtDouble
          case TypeId.Builtins.f128                         => baboonDecimal
          case TypeId.Builtins.str                          => dtString
          case TypeId.Builtins.bytes                        => dtUint8List
          case TypeId.Builtins.uid                          => dtString
          case TypeId.Builtins.tsu                          => dtDateTime
          case TypeId.Builtins.tso                          => baboonDateTimeOffset
          case other => throw new IllegalArgumentException(s"Unexpected: $other")
        }
      case TypeId.Builtins.map => dtMap
      case TypeId.Builtins.lst => dtList
      case TypeId.Builtins.set => dtSet
      case TypeId.Builtins.opt =>
        throw new IllegalArgumentException("opt should be handled via nullable T?, not as a raw type constructor")
      case uid: TypeId.User => asDtTypeDerefForeigns(uid, domain, evo)
      case other            => throw new IllegalArgumentException(s"Unexpected: $other")
    }
  }

  def toDtPkg(p: Pkg, version: Version, evolution: BaboonEvolution): DtPackageId = {
    toDtPkg(
      p,
      version,
      omitVersion = version == evolution.latest,
    )
  }

  def toDtPkg(p: Pkg, version: Version, omitVersion: Boolean): DtPackageId = {
    val verString = "v" + version.v.toString
      .split('.')
      .mkString("_")

    val base = p.path.map(_.toLowerCase)
    val segments = if (omitVersion) {
      base
    } else {
      base :+ verString
    }

    DtPackageId(segments)
  }

  private def asDtTypeDerefForeigns(tid: TypeId.User, domain: Domain, evolution: BaboonEvolution): DtType = {
    domain.defs.meta.nodes(tid) match {
      case DomainMember.User(_, defn: Typedef.Foreign, _, _) =>
        val fe    = defn.bindings("dart")
        val parts = fe.decl.split('.').toList
        assert(parts.length > 1)
        val pkg = parts.init
        val id  = parts.last
        DtType(DtPackageId(NEList.unsafeFrom(pkg)), id)
      case _ =>
        toDtTypeRefKeepForeigns(tid, domain, evolution)
    }
  }

  def toDtTypeRefKeepForeigns(tid: TypeId.User, domain: Domain, evolution: BaboonEvolution): DtType = {
    val version = domain.version
    val pkg     = toDtPkg(tid.pkg, version, evolution)

    tid.owner match {
      case Owner.Adt(id) =>
        val parentOwnerParts = renderOwner(id.owner)
        val parentPkg        = DtPackageId(pkg.parts ++ parentOwnerParts)
        DtType(parentPkg, tid.name.name, importAs = Some(toSnakeCase(id.name.name)))
      case other =>
        val ownerAsPrefix = renderOwner(other)
        DtType(DtPackageId(pkg.parts ++ ownerAsPrefix), tid.name.name)
    }
  }

  private def renderOwner(owner: Owner): Seq[String] = {
    owner match {
      case Owner.Toplevel => Seq.empty
      case Owner.Ns(path) => path.map(_.name.toLowerCase)
      case Owner.Adt(id)  => renderOwner(id.owner) :+ id.name.name.toLowerCase
    }
  }

  def effectiveDtPkg(owner: Owner, domain: Domain, evo: BaboonEvolution): DtPackageId = {
    val basePkg = toDtPkg(domain.id, domain.version, evo)
    owner match {
      case Owner.Toplevel  => basePkg
      case Owner.Ns(path)  => DtPackageId(NEList.unsafeFrom(basePkg.parts.toList ++ path.map(_.name.toLowerCase)))
      case Owner.Adt(id)   => effectiveDtPkg(id.owner, domain, evo)
    }
  }

  /** Convert a CamelCase or PascalCase name to snake_case for Dart file names */
  def toSnakeCase(name: String): String = {
    name.replaceAll("([A-Z]+)([A-Z][a-z])", "$1_$2")
      .replaceAll("([a-z\\d])([A-Z])", "$1_$2")
      .toLowerCase
  }

  private val dartKeywords: Set[String] = Set(
    "abstract", "as", "assert", "async", "await", "break", "case", "catch",
    "class", "const", "continue", "covariant", "default", "deferred", "do",
    "dynamic", "else", "enum", "export", "extends", "extension", "external",
    "factory", "false", "final", "finally", "for", "get", "hide", "if",
    "implements", "import", "in", "interface", "is", "late", "library",
    "mixin", "new", "null", "on", "operator", "part", "required", "rethrow",
    "return", "sealed", "set", "show", "static", "super", "switch", "sync",
    "this", "throw", "true", "try", "type", "typedef", "var", "void",
    "when", "while", "with", "yield",
  )

  def escapeDartKeyword(name: String): String = {
    if (dartKeywords.contains(name)) s"${name}_" else name
  }
}
