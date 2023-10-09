package io.septimalmind.baboon.parser.model.issues

import fastparse.Parsed
import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.*
import io.septimalmind.baboon.typer.BaboonTyper
import io.septimalmind.baboon.typer.BaboonTyper.FullRawDefn
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.graphs.ToposortError
import izumi.fundamentals.graphs.tools.cycles.LoopDetector
import izumi.fundamentals.platform.exceptions.Issue
import izumi.fundamentals.platform.exceptions.Issue.IssueContext
import izumi.fundamentals.platform.exceptions.IzThrowable.*
import izumi.fundamentals.platform.strings.IzString.toRichIterable

sealed trait BaboonIssue

object BaboonIssue {

  sealed trait BaboonBug {
    this: BaboonIssue =>
    def errorMessage: String = ""

    override def toString: String = {
      s"""
         |BABOON BUG!!!
         |$errorMessage
         |
         |Report: $issuesUrl
         |""".stripMargin
    }
  }

  //
  sealed trait IOIssue extends BaboonIssue

  case class CantReadInput(path: String, throwable: Throwable) extends IOIssue {
    override def toString: String = {
      s"""
         |Can't read form file: $path
         |Due to:
         |   ${throwable.stacktraceString}
         |""".stripMargin
    }
  }

  case class CantWriteOutput(path: String, throwable: Throwable)
      extends IOIssue {
    override def toString: String = {
      s"""
         |Can't write to file: $path
         |Due to:
         |   ${throwable.stacktraceString}
         |""".stripMargin
    }
  }

  //
  sealed trait ParserIssue extends BaboonIssue

  case class ParserFailed(error: Parsed.Failure) extends ParserIssue {
    override def toString: String = {
      val Array(line, character, _*) =
        error.extra.input.prettyIndex(error.index).split(":")
      s"""
         |Parser error occurred on:
         |   Line:     $line
         |   Position: $character
         |""".stripMargin
    }
  }

  //
  sealed trait TyperIssue extends BaboonIssue

  case class ScalarExpected(id: TypeId, meta: RawNodeMeta)
      extends TyperIssue
      with BaboonBug {
    override def errorMessage: String = {
      s"""
         |${extractLocation(meta)}
         |Scalar type is expected
         |Instead provided: ${id.toString}
         |""".stripMargin
    }
  }

  case class CollectionExpected(id: TypeId, meta: RawNodeMeta)
      extends TyperIssue
      with BaboonBug {
    override def errorMessage: String = {
      s"""
         |${extractLocation(meta)}
         |One of collection types expected: map, opt, list, set
         |Instead provided: ${id.toString}
         |""".stripMargin
    }
  }

  case class NonUniqueDomainVersions(
    duplicateDomainVersion: Map[Version, List[Domain]]
  ) extends TyperIssue {
    override def toString: String = {
      val stringProblems = duplicateDomainVersion
        .map {
          case (version, domains) =>
            s"""\tVersion: $version
           |\tModels:
           |${domains.niceList("\t\t")}
           |""".stripMargin
        }
        .mkString("\n")
      s"""
         |Duplicate domain versions have been found
         |$stringProblems
         |""".stripMargin
    }
  }

  case class EmptyDomainFamily(pkg: Pkg) extends TyperIssue with BaboonBug {
    override def errorMessage: String = {
      s"Empty model: ${pkg.toString}"
    }
  }

  case class NonUniqueLineages(nonUniqueLineages: Map[Pkg, List[BaboonLineage]])
      extends TyperIssue {
    override def toString: String = {
      s"""
         |Non unique models found:
         |${nonUniqueLineages.keys.toList.niceList()}
         |""".stripMargin
    }
  }

  case class EmptyFamily(input: List[BaboonParser.Input])
      extends TyperIssue
      with BaboonBug {
    override def errorMessage: String = {
      s"""
         |Empty models in files:
         |${input.niceList()}
         |""".stripMargin
    }
  }

  case class UnexpectedBuiltin(id: TypeId.Builtin, pkg: Pkg, meta: RawNodeMeta)
      extends TyperIssue {
    override def toString: String = {
      s"""
         |${extractLocation(meta)}
         |Model: ${pkg.toString}
         |The type ${id.toString} - is a built in type it cannot be used as a user defined type
         |""".stripMargin
    }
  }

  case class UnexpectedNonBuiltin(name: TypeName,
                                  pkg: Pkg,
                                  path: NEList[Scope[FullRawDefn]],
                                  meta: RawNodeMeta)
      extends TyperIssue {
    override def toString: String = {
      s"""
         |${extractLocation(meta)}
         |Model: ${pkg.toString}
         |The type ${name.name} is not supported
         |Consider using one of our builtin types: ${TypeId.Builtins.all
           .map(_.name.name)
           .mkString(" ")}
         |""".stripMargin
    }
  }

  case class MissingTypeId(domain: Pkg, missing: Set[TypeId], meta: RawNodeMeta)
      extends TyperIssue {
    override def toString: String = {
      s"""
         |${extractLocation(meta)}
         |Missing type ids: ${missing.toList.map(_.name.name).mkString}
         |""".stripMargin
    }
  }

  case class NonUniqueEnumBranches(
    duplicateEnumMembers: Map[String, List[EnumMember]],
    id: TypeId.User,
    meta: RawNodeMeta
  ) extends TyperIssue {
    override def toString: String = {
      val stringProblems = duplicateEnumMembers.values
        .map(_.niceList())
        .mkString("\n")
      s"""
         |${extractLocation(meta)}
         |Enum: ${id.toString}
         |Has members with identical names:
         |$stringProblems
         |""".stripMargin
    }
  }

  case class EmptyEnum(id: TypeId.User, meta: RawNodeMeta) extends TyperIssue {
    override def toString: String = {
      s"""
         |${extractLocation(meta)}
         |Found empty enum: ${id.toString}
         |""".stripMargin
    }
  }

  case class NonUniqueFields(id: TypeId.User,
                             duplicateFields: Map[String, List[Field]],
                             meta: RawNodeMeta)
      extends TyperIssue {
    override def toString: String = {
      val duplicateFieldsString = duplicateFields
        .map {
          case (fieldName, fields) =>
            s""" Name: $fieldName
           | Fields:
           |${fields.niceList()}
           |""".stripMargin
        }
        .mkString("\n")
      s"""
         |${extractLocation(meta)}
         |DTO: ${id.toString}
         |Has fields with identical names:
         |$duplicateFieldsString
         |""".stripMargin
    }
  }

  case class EmptyAdt(id: TypeId.User, meta: RawNodeMeta) extends TyperIssue {
    override def toString: String = {
      s"""
         |${extractLocation(meta)}
         |Found empty ADT: ${id.toString}
         |""".stripMargin
    }
  }

  case class EmptyGenericArgs(id: TypeId, meta: RawNodeMeta)
      extends TyperIssue {
    override def toString: String = {
      s"""
         |${extractLocation(meta)}
         |The type ${id.name.name} is expected to be used with type argument
         |""".stripMargin
    }
  }

  case class DuplicatedTypedefs(
    model: RawDomain,
    duplicateTypeDefs: Map[TypeId, List[DomainMember]],
  ) extends TyperIssue {
    override def toString: String = {
      val duplicateTypesString = duplicateTypeDefs.map {
        case (typeId, members) =>
          s"""
           |\t$typeId
           |\t${members.map(_.id.name)}
           |""".stripMargin
      }
      s"""
         |${extractLocation(model.header.meta)}
         |Duplicate type definitions have been found:
         |$duplicateTypesString
         |""".stripMargin
    }
  }

  case class CircularDependency(model: RawDomain, error: ToposortError[TypeId])
      extends TyperIssue {
    override def toString: String = {
      val stringProblem = Option(error)
        .collect {
          case ToposortError.UnexpectedLoop(_, matrix) =>
            matrix.links.map {
              case (typeId, dependencies) =>
                s"""
               |\ttype:         ${typeId.name}
               |\tdependencies: ${dependencies.map(_.name.name).mkString(", ")}
               |""".stripMargin
            }.mkString
        }
        .getOrElse("")
      s"""
         |${extractLocation(model.header.meta)}
         |Circular dependencies have been found: $stringProblem
         |""".stripMargin
    }
  }

  case class EmptyPackageId(header: RawHeader) extends TyperIssue {
    override def toString: String = {
      s"""
         |${extractLocation(header.meta)}
         |There is an empty package name
         |""".stripMargin
    }
  }

  case class NonUniqueTypedefs(
    duplicateTypedefs: Map[TypeId, List[DomainMember]],
    meta: RawNodeMeta
  ) extends TyperIssue {
    override def toString: String = {
      val stringProblems =
        duplicateTypedefs.values.flatten.map(_.id.name.name).mkString
      s"""
         |${extractLocation(meta)}
         |Duplicate type members found:
         |   $stringProblems
         |""".stripMargin
    }
  }

  case class NonUniqueScope(
    nonUniqueScopes: Map[Scope.ScopeName, List[Scope.NestedScope[FullRawDefn]]],
    meta: RawNodeMeta
  ) extends TyperIssue {
    override def toString: String = {
      val nusString =
        nonUniqueScopes.map(_._2.map(_.name.name).mkString(", ")).niceList()
      s"""
         |${extractLocation(meta)}
         |Duplicate scopes have been found:
         |$nusString
         |""".stripMargin
    }
  }

  case class UnexpectedScoping(e: List[Scope[FullRawDefn]], meta: RawNodeMeta)
      extends TyperIssue
      with BaboonBug {
    override def errorMessage: String = {
      s"""
         |${extractLocation(meta)}
         |Unexpected scoping: ${e.mkString}
         |""".stripMargin
    }
  }

  case class ScopeCannotBeEmpty(member: RawDefn) extends TyperIssue {
    override def toString: String = {
      val memberType = member match {
        case _: RawDto  => "DTO"
        case _: RawEnum => "Enum"
        case _: RawAdt  => "ADT"
      }
      s"""
         |${extractLocation(member.meta)}
         |Found an empty $memberType: ${member.name.name}
         |""".stripMargin
    }
  }

  case class BadEnumName(name: String, meta: RawNodeMeta) extends TyperIssue {
    override def toString: String = {
      s"""
         |${extractLocation(meta)}
         |Bad enum name: $name
         |""".stripMargin
    }
  }

  case class BadFieldName(name: String, meta: RawNodeMeta) extends TyperIssue {
    override def toString: String = {
      s"""
         |${extractLocation(meta)}
         |Bad field name: $name
         |""".stripMargin
    }
  }

  case class BadTypeName(name: String, meta: RawNodeMeta) extends TyperIssue {
    override def toString: String = {
      s"""
         |${extractLocation(meta)}
         |Bad type name: $name
         |""".stripMargin
    }
  }

  case class BadInheritance(
    bad: Map[TypeId.User, List[(Set[TypeId.User], BaboonTyper.ScopedDefn)]],
    meta: RawNodeMeta
  ) extends TyperIssue
      with BaboonBug {
    override def errorMessage: String = {
      val badStr = bad
        .map {
          case (id, deps) =>
            val depsStr = deps
              .map {
                case (deps, scope) =>
                  s"""
             |Scope: ${scope.toString}
             |Dependencies: ${deps.map(_.toString).mkString(", ")}
             |""".stripMargin
              }
              .niceList("\t")
            s"""
           |Type: ${id.toString}
           |$depsStr
           |""".stripMargin
        }
        .niceList("\t\t")
      s"""
         |${extractLocation(meta)}
         |Bad inheritance:
         |$badStr
         |""".stripMargin
    }
  }

  case class CircularInheritance(error: ToposortError[TypeId.User],
                                 meta: RawNodeMeta)
      extends TyperIssue {
    override def toString: String = {
      val errorString = Option(error)
        .collect {
          case ToposortError.UnexpectedLoop(_, matrix) =>
            matrix.links.map {
              case (typeId, children) =>
                s"""
               |\ttype:     ${typeId.name.name}
               |\tchildren: ${children.toList.map(_.name.name).mkString(", ")}
               |""".stripMargin
            }.mkString
        }
        .getOrElse("")
      s"""
         |${extractLocation(meta)}
         |Circular inheritance have been found:
         |$errorString
         |""".stripMargin
    }
  }

  case class NameNotFound(pkg: Pkg, name: ScopedRef, meta: RawNodeMeta)
      extends TyperIssue {
    override def toString: String = {
      s"""
         |${extractLocation(meta)}
         |Type ${name.path.head.name} not found
         |""".stripMargin
    }
  }

  case class UnexpectedScopeLookup(b: Scope[FullRawDefn], meta: RawNodeMeta)
      extends TyperIssue {
    override def toString: String = {
      s"""
         |${extractLocation(meta)}
         |Unexpected scope: ${b.toString}
         |""".stripMargin
    }
  }

  case class NamSeqeNotFound(names: Seq[RawTypeName],
                             scope: Scope.SubScope[FullRawDefn],
                             meta: RawNodeMeta)
      extends TyperIssue {
    override def toString: String = {
      s"""
         |${extractLocation(meta)}
         |Members: ${names.map(_.name).mkString}
         |Are not found in scope: ${scope.name.name}
         |""".stripMargin
    }
  }

  case class DuplicatedTypes(dupes: Set[TypeId], meta: RawNodeMeta)
      extends TyperIssue {
    override def toString: String = {
      s"""
         |${extractLocation(meta)}
         |Duplicate types have been found: ${dupes
           .map(_.name.name)
           .mkString(", ")}
         |""".stripMargin
    }
  }

  case class WrongParent(id: TypeId.User, id1: TypeId, meta: RawNodeMeta)
      extends TyperIssue {
    override def toString: String = {
      s"""
         |${extractLocation(meta)}
         |DTO parent is expected
         |Instead provided:
         |   name: ${id.toString}
         |   type: ${id1.name.name}
         |""".stripMargin
    }
  }

  //
  sealed trait EvolutionIssue extends BaboonIssue

  case class BrokenComparison(versions: List[Version])
      extends EvolutionIssue
      with BaboonBug {
    override def errorMessage: String = {
      s"""
         |Broken comparison
         |Versions:
         |${versions.map(v => "\t" + v.toString).mkString("\n")}
         |""".stripMargin
    }
  }

  case class UnexpectedDiffType(o: TypedefDiff, expected: String)
      extends EvolutionIssue
      with BaboonBug {
    override def errorMessage: String = {
      s"""
         |$expected is expected
         |Instead provided:
         |${o.ops.map(_.toString).mkString("\n")}
         |""".stripMargin
    }
  }

  case class NonUniqueDiff(evolutions: Map[EvolutionStep, List[BaboonDiff]])
      extends EvolutionIssue
      with BaboonBug {
    override def errorMessage: String = {
      val evolutionsStr = evolutions
        .map {
          case (evolution, differences) =>
            s"""
           |Evolution: from${evolution.from} to ${evolution.to}
           |Differences:
           |${differences.map(d => "\t" + d.toString)}
           |""".stripMargin
        }
        .mkString("\n")
      s"""
         |Non unique differences:
         |$evolutionsStr
         |""".stripMargin
    }
  }

  case class NonUniqueRuleset(
    evolutions: Map[EvolutionStep, List[BaboonRuleset]]
  ) extends EvolutionIssue
      with BaboonBug {
    override def errorMessage: String = {
      val evolutionsStr = evolutions
        .map {
          case (evolution, ruleset) =>
            s"""
           |Evolution: from${evolution.from} to ${evolution.to}
           |Ruleset:
           |${ruleset.map(d => "\t" + d.toString)}
           |""".stripMargin
        }
        .mkString("\n")
      s"""
         |Non unique rule sets:
         |$evolutionsStr
         |""".stripMargin
    }
  }

  case class IncomparableTypedefs(old: DomainMember, newMember: DomainMember)
      extends EvolutionIssue {
    override def toString: String = {
      s"""
         |In order to compare two types for evolution
         |they should be user defined
         |Instead provided:
         |   ${old.id.toString}
         |   ${newMember.id.toString}
         |""".stripMargin
    }
  }

  case class NonUniqueDiffs(nonUniqueDiffs: Map[TypeId, List[TypedefDiff]])
      extends EvolutionIssue
      with BaboonBug {
    override def errorMessage: String = {
      val nonUniqueDiffsStr = nonUniqueDiffs.map {
        case (typeId, diffs) =>
          s"""
           |Type:        ${typeId.name}
           |Differences:
           |${diffs.map(d => "\t" + d.toString).mkString("\n")}
           |""".stripMargin
      }
      s"""
         |Non unique differences:
         |$nonUniqueDiffsStr
         |""".stripMargin
    }
  }

  case class MismatchingTypedefs(o1: Typedef.User, o2: Typedef.User)
      extends EvolutionIssue
      with BaboonBug {
    override def errorMessage: String = {
      s"""
         |Mismatching type defs:
         |   ${o1.toString}
         |   ${o2.toString}
         |""".stripMargin
    }
  }

  //
  sealed trait VerificationIssue extends BaboonIssue

  case class MissingTypeDef(domain: Domain, missing: Set[TypeId])
      extends VerificationIssue {
    override def toString: String = {
      s"""
         |Model: ${domain.id.toString}
         |Has missing type definitions: ${missing.map(_.name.name).mkString}
         |""".stripMargin
    }
  }

  case class ReferentialCyclesFound(domain: Domain,
                                    loops: Set[LoopDetector.Cycles[TypeId]])
      extends VerificationIssue {
    override def toString: String = {
      val stringLoops = loops.toList
        .map { cycle =>
          s"""
           |\tType: ${cycle.node.name.name}
           |\tLoop: ${cycle.loops.flatMap(_.loop.map(_.name.name)).mkString}
           |""".stripMargin
        }
        .mkString("\n")
      s"""
         |Model: ${domain.id.toString}
         |Referential cycles have been found:
         |$stringLoops
         |""".stripMargin
    }
  }

  case class IncorrectRootFound(domain: Domain, badRoots: Seq[TypeId.User])
      extends VerificationIssue

  case class ConflictingDtoFields(dto: Typedef.Dto,
                                  dupes: Map[String, List[Field]],
                                  meta: RawNodeMeta)
      extends VerificationIssue {
    override def toString: String = {
      val stringDuplicates = dupes
        .map {
          case (name, fields) =>
            s"""
           |\tName: $name
           |\tTypes: ${fields.map(_.tpe.id.toString)}
           |""".stripMargin
        }
        .mkString("\n")
      s"""
         |${extractLocation(meta)}
         |In DTO: ${dto.id.name.name}
         |Conflicting fields have been found:
         |$stringDuplicates
         |""".stripMargin
    }
  }

  case class ConflictingEnumBranches(uEnum: Typedef.Enum,
                                     dupes: Map[String, NEList[EnumMember]],
                                     meta: RawNodeMeta)
      extends VerificationIssue {
    override def toString: String = {
      s"""
         |${extractLocation(meta)}
         |In Enum: ${uEnum.id.name.name}
         |Conflicting members have been found:
         |   ${dupes.values.flatten.map(_.name).mkString(", ")}
         |""".stripMargin
    }
  }

  case class ConflictingAdtBranches(adt: Typedef.Adt,
                                    dupes: Map[String, NEList[TypeId.User]],
                                    meta: RawNodeMeta)
      extends VerificationIssue {
    override def toString: String = {
      val stringDupes = dupes
        .map {
          case (name, branches) =>
            s"""
           |\tName:     $name
           |\tBranches: ${branches.map(_.name.name).mkString(", ")}
           |""".stripMargin
        }
        .mkString("\n")
      s"""
         |${extractLocation(meta)}
         |In ADT: ${adt.id.name.name}
         |Conflicting branches have been found:
         |$stringDupes
         |""".stripMargin
    }
  }

  case class ConflictingTypeIds(domain: Domain,
                                dupes: Map[String, Iterable[DomainMember]])
      extends VerificationIssue {
    override def toString: String = {
      val stringDupes = dupes
        .map {
          case (name, members) =>
            s"""
           |\tName: $name
           |\tMembers: ${members.map(_.id.name.name).mkString(", ")}
           |""".stripMargin
        }
        .mkString("\n")
      s"""
         |In model: ${domain.id.toString}
         |Conflicting types have been found:
         |$stringDupes
         |""".stripMargin
    }
  }

  case class EmptyEnumDef(e: Typedef.Enum, meta: RawNodeMeta)
      extends VerificationIssue {
    override def toString: String = {
      s"""
         |${extractLocation(meta)}
         |Empty enum: ${e.id.toString}
         |""".stripMargin
    }
  }

  case class EmptyAdtDef(a: Typedef.Adt, meta: RawNodeMeta)
      extends VerificationIssue {
    override def toString: String = {
      s"""
         |${extractLocation(meta)}
         |Empty ADT: ${a.id.toString}
         |""".stripMargin
    }
  }

  case class MissingEvoDiff(prev: Domain,
                            next: Domain,
                            missingDiffs: Set[TypeId])
      extends VerificationIssue
      with BaboonBug {
    override def errorMessage: String = {
      s"""
         |Missing differences:
         |${missingDiffs.niceList()}
         |In model:
         |   Previous version: ${prev.id.toString}
         |   Next version:     ${next.id.toString}
         |""".stripMargin
    }
  }

  case class MissingEvoConversion(prev: Domain,
                                  next: Domain,
                                  missingConversions: Set[TypeId],
                                  extraConversions: Set[TypeId])
      extends VerificationIssue
      with BaboonBug {
    override def errorMessage: String = {
      s"""
         |Missing evolution conversion
         |Missing conversions:
         |${missingConversions.niceList()}
         |${extraConversions.niceList()}
         |In model:
         |   Previous version: ${prev.id.toString}
         |   Next version:     ${next.id.toString}
         |""".stripMargin
    }
  }

  case class BrokenConversion(c: Conversion)
      extends VerificationIssue
      with BaboonBug {
    override def errorMessage: String = {
      s"""
         |Removed type: ${c.sourceTpe.toString}
         |Is not present as removed type in conversion:
         |${c.toString}
         |""".stripMargin
    }
  }

  sealed trait ConversionIssue
  object ConversionIssue {
    case object TypeMismatch extends ConversionIssue
    case object RemovedEnumBranches extends ConversionIssue
    case object RemovedAdtBranches extends ConversionIssue
    case object RemovedDtoFields extends ConversionIssue
    case object IncorrectFields extends ConversionIssue
    case object ConflictingFields extends ConversionIssue
  }
  case class IncorrectConversionApplication(c: Conversion,
                                            o: DomainMember,
                                            n: DomainMember,
                                            issue: ConversionIssue)
      extends VerificationIssue
      with BaboonBug {
    override def errorMessage: String = {
      val location = (o, n) match {
        case (old: DomainMember.User, newMember: DomainMember.User) =>
          s"""
             |Old member location:
             |   ${extractLocation(old.meta)}
             |
             |New member location:
             |   ${extractLocation(newMember.meta)}
             |""".stripMargin
        case _ => ""
      }
      location +
        s"""
           |Incorrect conversion: ${c.toString}
           |Member:
           |   Old version: ${o.id.toString}
           |   New version: ${n.id.toString}
           |Issue: ${issue.toString}
           |""".stripMargin
    }
  }

  case class PathologicGenerics(dto: Typedef.Dto,
                                badFields: List[Field],
                                meta: RawNodeMeta)
      extends VerificationIssue {
    override def toString: String = {
      s"""
         |${extractLocation(meta)}
         |DTO: ${dto.id.toString}
         |Has nested option fields:
         |   ${badFields.map(_.toString).mkString("\n")}
         |""".stripMargin
    }
  }

  case class SetsCantContainGenerics(dto: Typedef.Dto,
                                     badFields: List[Field],
                                     meta: RawNodeMeta)
      extends VerificationIssue {
    override def toString: String = {
      s"""
         |${extractLocation(meta)}
         |DTO: ${dto.id.toString}
         |Has set fields with generic type parameter:
         |   ${badFields.map(_.toString).mkString("\n")}
         |""".stripMargin
    }
  }

  case class MapKeysShouldNotBeGeneric(dto: Typedef.Dto,
                                       badFields: List[Field],
                                       meta: RawNodeMeta)
      extends VerificationIssue {
    override def toString: String = {
      s"""
         |${extractLocation(meta)}
         |DTO: ${dto.id.toString}
         |Has map fields with generic key:
         |   ${badFields.map(_.toString).mkString("\n")}
         |""".stripMargin
    }
  }

  //
  sealed trait TranslationIssue extends BaboonIssue
  case class NonUniqueOutputFiles(c: Map[String, List[String]])
      extends TranslationIssue {
    override def toString: String = {
      s"""
         |Non unique output files:
         |   ${c.keySet.mkString("\n")}
         |""".stripMargin
    }
  }

  case class TranslationBug()(implicit val context: IssueContext)
      extends TranslationIssue
      with BaboonBug
      with Issue

  private def extractLocation(meta: RawNodeMeta): String = {
    meta.pos match {
      case full: InputPointer.Full =>
        s"""
           |From => (${full.file.asString}:${full.start.line}) position=${full.start.column}
           |To   => (${full.file.asString}:${full.stop.line})  position=${full.stop.column}
           |""".stripMargin
      case offset: InputPointer.Offset =>
        s"""
           |In => (${offset.file.asString}:${offset.start.line}) position=${offset.start.column}
           |""".stripMargin
      case file: InputPointer.JustFile =>
        s"""
           |In file: ${file.file.asString}
           |""".stripMargin
      case InputPointer.Undefined => ""
    }
  }

  private val issuesUrl = "https://github.com/7mind/baboon/issues"

}
