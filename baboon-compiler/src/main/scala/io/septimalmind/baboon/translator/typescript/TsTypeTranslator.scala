package io.septimalmind.baboon.translator.typescript

import io.septimalmind.baboon.translator.typescript.TsTypes.*
import io.septimalmind.baboon.translator.typescript.TsValue.{TsModuleId, TsType}
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.Quote

class TsTypeTranslator {

  def asTsRef(tpe: TypeRef, domain: Domain, evo: BaboonEvolution): TextTree[TsValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        q"${asTsType(id, domain, evo)}"

      case TypeRef.Constructor(id, args) =>
        id match {
          case TypeId.Builtins.opt =>
            val inner = asTsRef(args.head, domain, evo)
            q"($inner | undefined)"
          case TypeId.Builtins.lst =>
            val inner = asTsRef(args.head, domain, evo)
            q"ReadonlyArray<$inner>"
          case TypeId.Builtins.set =>
            val inner = asTsRef(args.head, domain, evo)
            q"ReadonlySet<$inner>"
          case TypeId.Builtins.map =>
            val key   = asTsRef(args.head, domain, evo)
            val value = asTsRef(args.last, domain, evo)
            q"ReadonlyMap<$key, $value>"
          case o => throw new IllegalArgumentException(s"Unexpected collection type: $o")
        }
    }
  }

  def asTsType(tpe: TypeId, domain: Domain, evo: BaboonEvolution): TsType = {
    tpe match {
      case b: TypeId.BuiltinScalar =>
        b match {
          case TypeId.Builtins.bit   => tsBoolean
          case TypeId.Builtins.i08   => tsNumber
          case TypeId.Builtins.i16   => tsNumber
          case TypeId.Builtins.i32   => tsNumber
          case TypeId.Builtins.i64   => tsBigint
          case TypeId.Builtins.u08   => tsNumber
          case TypeId.Builtins.u16   => tsNumber
          case TypeId.Builtins.u32   => tsNumber
          case TypeId.Builtins.u64   => tsBigint
          case TypeId.Builtins.f32   => tsNumber
          case TypeId.Builtins.f64   => tsNumber
          case TypeId.Builtins.f128  => baboonDecimal
          case TypeId.Builtins.str   => tsString
          case TypeId.Builtins.bytes => tsUint8Array
          case TypeId.Builtins.uid   => tsString
          case TypeId.Builtins.tsu   => baboonDateTimeUtc
          case TypeId.Builtins.tso   => baboonDateTimeOffset
          case other => throw new IllegalArgumentException(s"Unexpected builtin scalar: $other")
        }
      case TypeId.Builtins.map => TsType(predefModule, "ReadonlyMap", predef = true)
      case TypeId.Builtins.lst => TsType(predefModule, "ReadonlyArray", predef = true)
      case TypeId.Builtins.set => TsType(predefModule, "ReadonlySet", predef = true)
      case TypeId.Builtins.opt => TsType(predefModule, "undefined", predef = true)
      case uid: TypeId.User    => asTsTypeDerefForeigns(uid, domain, evo)
      case other => throw new IllegalArgumentException(s"Unexpected type: $other")
    }
  }

  def toTsModule(p: Pkg, version: Version, evolution: BaboonEvolution): TsModuleId = {
    toTsModule(p, version, omitVersion = version == evolution.latest)
  }

  def toTsModule(p: Pkg, version: Version, omitVersion: Boolean): TsModuleId = {
    val verString = "v" + version.v.toString.split('.').mkString("_")
    val base = p.path.map(_.toLowerCase)
    val segments = if (omitVersion) base else base :+ verString
    TsModuleId(NEList.unsafeFrom(segments.toList))
  }

  private def asTsTypeDerefForeigns(tid: TypeId.User, domain: Domain, evolution: BaboonEvolution): TsType = {
    domain.defs.meta.nodes(tid) match {
      case DomainMember.User(_, defn: Typedef.Foreign, _, _) =>
        val fe = defn.bindings("typescript")
        TsType(TsModuleId(NEList("foreign")), fe.decl, predef = true)
      case _ =>
        toTsTypeRefKeepForeigns(tid, domain, evolution)
    }
  }

  def toTsTypeRefKeepForeigns(tid: TypeId.User, domain: Domain, evolution: BaboonEvolution): TsType = {
    val version = domain.version
    val module  = toTsModule(tid.pkg, version, evolution)

    val ownerAsPrefix = renderOwner(tid.owner)
    val escapedName = escapeTsKeyword(tid.name.name)

    val fullModule = tid.owner match {
      case Owner.Adt(_) =>
        TsModuleId(NEList.unsafeFrom((module.parts ++ ownerAsPrefix).toList))
      case _ =>
        val fileName = toTsFileName(tid.name.name)
        TsModuleId(NEList.unsafeFrom((module.parts ++ ownerAsPrefix :+ fileName).toList))
    }
    TsType(fullModule, escapedName)
  }

  private def renderOwner(owner: Owner): Seq[String] = {
    owner match {
      case Owner.Toplevel => Seq.empty
      case Owner.Ns(path) => path.map(_.name.toLowerCase)
      case Owner.Adt(id)  => renderOwner(id.owner) :+ toTsFileName(id.name.name)
    }
  }

  def escapeTsKeyword(s: String): String = {
    s match {
      case "break" | "case" | "catch" | "continue" | "debugger" | "default" | "delete" | "do" |
           "else" | "finally" | "for" | "function" | "if" | "in" | "instanceof" | "new" |
           "return" | "switch" | "this" | "throw" | "try" | "typeof" | "var" | "void" | "while" |
           "with" | "class" | "const" | "enum" | "export" | "extends" | "import" | "super" |
           "implements" | "interface" | "let" | "package" | "private" | "protected" | "public" |
           "static" | "yield" | "await" | "async" | "of" | "type" | "from" | "as" | "is" => s"${s}_"
      case _ => s
    }
  }

  def toTsFileName(s: String): String = {
    camelToKebab(s)
  }

  def camelToKebab(s: String): String = {
    val result = new StringBuilder
    var i = 0
    while (i < s.length) {
      val c = s.charAt(i)
      if (c.isUpper) {
        if (i > 0 && s.charAt(i - 1).isLower) {
          result += '-'
        } else if (i > 0 && i + 1 < s.length && s.charAt(i + 1).isLower && s.charAt(i - 1).isUpper) {
          result += '-'
        }
        result += c.toLower
      } else {
        result += c
      }
      i += 1
    }
    result.toString()
  }
}
