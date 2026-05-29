package io.septimalmind.baboon.translator.typescript

import io.septimalmind.baboon.CompilerTarget.TsTarget
import io.septimalmind.baboon.translator.typescript.TsTypes.*
import io.septimalmind.baboon.translator.typescript.TsValue.{TsModuleId, TsType}
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.Quote

class TsTypeTranslator(target: TsTarget) {
  private val mapsAsRecords: Boolean       = target.language.mapsAsRecords
  private val timestampsUtcMode: String    = target.language.timestampsUtcMode
  private val timestampsOffsetMode: String = target.language.timestampsOffsetMode

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

      case TypeRef.Constructor(id, args) if id == TypeId.Builtins.map && mapsAsRecords && isStringKey(args.head) =>
        val valRef = asTsRef(args.toSeq(1), domain, evo, pkgBase)
        q"$tsRecord<$tsString, $valRef>"

      case TypeRef.Constructor(id, args) =>
        val tpe   = asTsType(id, domain, evo, pkgBase)
        val targs = args.map(asTsRef(_, domain, evo, pkgBase))
        q"$tpe<${targs.toSeq.join(", ")}>"
      case _: TypeRef.Any => q"$tsBaboonAnyOpaque"
    }
  }

  def isStringKeyMap(tpe: TypeRef): Boolean = {
    tpe match {
      case TypeRef.Constructor(TypeId.Builtins.map, args) => mapsAsRecords && isStringKey(args.head)
      case _                                              => false
    }
  }

  private def isStringKey(tpe: TypeRef): Boolean = {
    tpe match {
      case TypeRef.Scalar(TypeId.Builtins.str) => true
      case TypeRef.Scalar(TypeId.Builtins.uid) => true
      case _                                   => false
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
          case TypeId.Builtins.tsu =>
            timestampsUtcMode match {
              case "string" => tsString
              case "date"   => tsDate
              case _        => tsBaboonDateTimeUtc
            }
          case TypeId.Builtins.tso =>
            timestampsOffsetMode match {
              case "string" => tsString
              case "date"   => tsDate
              case _        => tsBaboonDateTimeOffset
            }
          case TypeId.Builtins.bit => tsBoolean

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
    val module = toTsModule(tid, domain, evolution, pkgBase)
    val isTypeOnly = domain.defs.meta.nodes.get(tid).exists {
      case DomainMember.User(_, _: Typedef.Contract, _, _) => true
      case DomainMember.User(_, _: Typedef.Service, _, _)  => true
      case _                                               => false
    }
    // ADT-branch DTOs are prefixed with the owning ADT name (`<Adt>_<Branch>`) to avoid module-/
    // barrel-level collisions between same-named branches of different ADTs (and between a branch
    // and a top-level type). Branches stay inline in the ADT file; only the TS symbol changes — the
    // on-wire branch name (JSON envelope key, type identifier, UEBA tag) is unaffected.
    val symbolName = tid.owner match {
      case Owner.Adt(adtId) => s"${adtId.name.name.capitalize}_${tid.name.name.capitalize}"
      case _                => tid.name.name.capitalize
    }
    TsType(module, symbolName, typeOnly = isTypeOnly)
  }

  def toTsModule(
    tid: TypeId.User,
    domain: Domain,
    evolution: BaboonEvolution,
    pkgBase: List[String],
    suffix: String = "",
  ): TsModuleId = {
    val version         = domain.version
    val pathToModule    = tid.pkg.path.toList.map(_.toLowerCase)
    val versionPathPart = if (version != evolution.latest) List(version.format(prefix = "v", delimiter = "_")) else Nil
    // A service type lives at `<serviceDir>/service` (file `service.ts`), where
    // serviceDir = nsPrefix.lower ++ [kebab(serviceName)]. References to the
    // service interface must resolve to that module while keeping the exported
    // PascalCase symbol. All other types keep the owner-derived module path.
    val serviceTail = domain.defs.meta.nodes.get(tid) match {
      case Some(DomainMember.User(_, svc: Typedef.Service, _, _)) => Some(serviceDirSegments(svc) :+ "service")
      case _                                                      => None
    }
    val ownerPath = renderOwner(tid.owner, domain)
    val name      = typeModuleName(tid).map(name => s"$name$suffix").toList
    val fullPath = serviceTail match {
      case Some(tail) => pkgBase ++ pathToModule ++ versionPathPart ++ tail
      case None       => pkgBase ++ pathToModule ++ versionPathPart ++ ownerPath ++ name
    }
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

  private def renderOwner(owner: Owner, domain: Domain): List[String] = {
    owner match {
      case Owner.Toplevel => Nil
      case Owner.Ns(path) => renderNsOwnerPath(path, domain)
      case Owner.Adt(id)  => renderOwner(id.owner, domain) :+ id.name.name
    }
  }

  /** Render the path segments of an `Owner.Ns`, kebab-casing exactly the one
    * segment that names a service while lowercasing every other segment.
    *
    * A method message type (`in`/`out`/`err`) lives at owner
    * `Owner.Ns(nsPrefix ++ [serviceName, methodName])`. The on-disk layout
    * places it under a per-service directory whose name is the KEBAB-cased
    * service name; the method directory keeps the lowercased method name.
    * Regular namespaces and ADTs carry no service in their path and must be
    * lowercased unchanged (`ns FooBar` -> `foobar`, never `foo-bar`).
    *
    * The shared rule lives here so file emission (`getOutputPath`/
    * `getOutputModule`), wiring/client paths, and type references all derive
    * the service segment identically -- divergence breaks `import` resolution.
    */
  def renderNsOwnerPath(path: Seq[TypeName], domain: Domain): List[String] = {
    val names = path.map(_.name).toList
    serviceSegmentIndex(names, domain) match {
      case Some(idx) =>
        names.zipWithIndex.map {
          case (n, i) => if (i == idx) camelToKebab(n) else n.toLowerCase
        }
      case None =>
        names.map(_.toLowerCase)
    }
  }

  /** Kebab-cased per-service directory segments for a service definition:
    * its namespace prefix lowercased, then the service name kebab-cased.
    */
  def serviceDirSegments(service: Typedef.Service): List[String] = {
    service.id.owner.asPseudoPkg.toList.map(_.toLowerCase) :+ camelToKebab(service.id.name.name)
  }

  /** Index into `names` of the segment that names a service, when the path is
    * nested under one. The service scope is `service.id.owner.asPseudoPkg ++
    * [service.id.name.name]`; when that sequence is a strict prefix of
    * `names`, the service-name segment sits at `owner.asPseudoPkg.size`.
    */
  private def serviceSegmentIndex(names: List[String], domain: Domain): Option[Int] = {
    domain.defs.meta.nodes.valuesIterator.collectFirst {
      case DomainMember.User(_, svc: Typedef.Service, _, _)
          if {
            val scope = svc.id.owner.asPseudoPkg.toList :+ svc.id.name.name
            names.startsWith(scope) && names.size > scope.size
          } =>
        svc.id.owner.asPseudoPkg.size
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
