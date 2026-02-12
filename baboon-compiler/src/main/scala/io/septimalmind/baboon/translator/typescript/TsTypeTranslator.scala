package io.septimalmind.baboon.translator.typescript

import io.septimalmind.baboon.translator.typescript.TsTypes.*
import io.septimalmind.baboon.translator.typescript.TsValue.{TsModuleId, TsType}
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.Quote

class TsTypeTranslator {
  def asTsRef(
    tpe: TypeRef,
    domain: Domain,
    evo: BaboonEvolution,
    pkgBase: List[String] = Nil,
  ): TextTree[TsValue] = {
    tpe match {
      case TypeRef.Scalar(uid: TypeId.User) =>
        domain.defs.meta.nodes(uid) match {
          case DomainMember.User(_, f: Typedef.Foreign, _, _) =>
            f.bindings.get(BaboonLang.Typescript) match {
              case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                asTsRef(aliasedRef, domain, evo, pkgBase)
              case _ =>
                q"${asTsType(uid, domain, evo, pkgBase)}"
            }
          case _ =>
            q"${asTsType(uid, domain, evo, pkgBase)}"
        }

      case TypeRef.Scalar(id) =>
        q"${asTsType(id, domain, evo, pkgBase)}"

      case TypeRef.Constructor(id, args) if id == TypeId.Builtins.opt =>
        q"${asTsRef(args.head, domain, evo, pkgBase)} | undefined"

      case TypeRef.Constructor(id, args) =>
        val tpe   = asTsType(id, domain, evo, pkgBase)
        val targs = args.map(asTsRef(_, domain, evo, pkgBase))
        q"$tpe<${targs.toSeq.join(", ")}>"
    }
  }

  def asTsType(
    tpe: TypeId,
    domain: Domain,
    evo: BaboonEvolution,
    pkgBase: List[String] = Nil,
  ): TsType = {
    tpe match {
      case b: TypeId.BuiltinScalar =>
        b match {
          case TypeId.Builtins.i08 | TypeId.Builtins.u08 => tsNumber
          case TypeId.Builtins.i16 | TypeId.Builtins.u16 => tsNumber
          case TypeId.Builtins.i32 | TypeId.Builtins.u32 => tsNumber
          case TypeId.Builtins.i64 | TypeId.Builtins.u64 => tsBigInt
          case TypeId.Builtins.f32                       => tsNumber
          case TypeId.Builtins.f64                       => tsNumber
          case TypeId.Builtins.f128                      => tsBaboonDecimal
          case TypeId.Builtins.str                       => tsString
          case TypeId.Builtins.bytes                     => tsBytes
          case TypeId.Builtins.uid                       => tsString
          case TypeId.Builtins.tsu                       => tsBaboonDateTimeUtc
          case TypeId.Builtins.tso                       => tsBaboonDateTimeOffset
          case TypeId.Builtins.bit                       => tsBoolean

          case other => throw new IllegalArgumentException(s"Unexpected: $other")
        }
      case TypeId.Builtins.map => tsMap
      case TypeId.Builtins.lst => tsArray
      case TypeId.Builtins.set => tsSet
      case uid: TypeId.User    => asTsTypeDerefForeign(uid, domain, evo, pkgBase)
      case other               => throw new IllegalArgumentException(s"Unexpected: $other")
    }
  }

  def asTsTypeDerefForeign(
    tid: TypeId.User,
    domain: Domain,
    evolution: BaboonEvolution,
    pkgBase: List[String],
  ): TsType = {
    derefForeign(tid, domain, evolution, pkgBase)
  }

  def asTsTypeKeepForeigns(
    tid: TypeId.User,
    domain: Domain,
    evolution: BaboonEvolution,
    pkgBase: List[String],
  ): TsType = {
    val module = toTsModule(tid, domain.version, evolution, pkgBase)
    TsType(module, s"${tid.name.name.capitalize}")
  }

  def toTsModule(
    tid: TypeId.User,
    version: Version,
    evolution: BaboonEvolution,
    pkgBase: List[String],
    suffix: String = "",
  ): TsModuleId = {
    val pathToModule    = tid.pkg.path.toList
    val versionPathPart = if (version != evolution.latest) List(version.format(prefix = "v", delimiter = "_")) else Nil
    val ownerPath       = renderOwner(tid.owner)
    val name            = typeModuleName(tid).map(name => s"$name$suffix").toList
    val fullPath        = pkgBase ++ pathToModule ++ versionPathPart ++ ownerPath ++ name
    TsModuleId(fullPath, if (version == evolution.latest) None else Some(version))
  }

  def toTsModule(p: Pkg, version: Version, evolution: BaboonEvolution, pkgBase: List[String]): TsModuleId = {
    toTsModule(
      p,
      version,
      omitVersion = version == evolution.latest,
      pkgBase,
    )
  }

  private def toTsModule(p: Pkg, version: Version, omitVersion: Boolean, pkgBase: List[String]): TsModuleId = {
    val verString = "v" + version.v.toString
      .split('.')
      .mkString("_")

    val base = p.path.map(_.toLowerCase)
    val segments = if (omitVersion) {
      pkgBase ++ base
    } else {
      pkgBase ++ base :+ verString
    }

    TsModuleId(segments)
  }

  private def derefForeign(
    tid: TypeId.User,
    domain: Domain,
    evolution: BaboonEvolution,
    pkgBase: List[String],
  ): TsType = {
    domain.defs.meta.nodes(tid) match {
      case DomainMember.User(_, defn: Typedef.Foreign, _, _) =>
        defn.bindings.get(BaboonLang.Typescript) match {
          case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.Custom(decl, _))) =>
            TsType(TsModuleId(List("foreign")), decl, predef = true)
          case _ =>
            asTsTypeKeepForeigns(tid, domain, evolution, pkgBase)
        }
      case _ =>
        asTsTypeKeepForeigns(tid, domain, evolution, pkgBase)
    }
  }

  private def typeModuleName(tid: TypeId.User): Option[String] = {
    tid.owner match {
      case _: Owner.Adt => None
      case _            => Some(tid.name.name)
    }
  }

  private def renderOwner(owner: Owner): List[String] = {
    owner match {
      case Owner.Toplevel => Nil
      case Owner.Ns(path) => path.map(_.name.toLowerCase).toList
      case Owner.Adt(id)  => renderOwner(id.owner) :+ id.name.name
    }
  }

  def escapeTsKeyword(s: String): String = {
    s match {
      case "break" | "case" | "catch" | "continue" | "debugger" | "default" | "delete" | "do" | "else" | "finally" | "for" | "function" | "if" | "in" | "instanceof" |
          "new" | "return" | "switch" | "this" | "throw" | "try" | "typeof" | "var" | "void" | "while" | "with" | "class" | "const" | "enum" | "export" | "extends" |
          "import" | "super" | "implements" | "interface" | "let" | "package" | "private" | "protected" | "public" | "static" | "yield" | "await" | "async" | "of" |
          "type" | "from" | "as" | "is" =>
        s"${s}_"
      case _ => s
    }
  }

  def toTsFileName(s: String): String = {
    camelToKebab(s)
  }

  def camelToKebab(s: String): String = {
    val result = new StringBuilder
    var i      = 0
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
