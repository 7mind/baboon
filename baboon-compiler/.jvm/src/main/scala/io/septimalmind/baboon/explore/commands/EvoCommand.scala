package io.septimalmind.baboon.explore.commands

import io.septimalmind.baboon.diff.OpDiffFormatter
import io.septimalmind.baboon.explore.{Colors, EitherF, ExploreContext}
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.Conversion.*

object EvoCommand extends Command {
  def name: String = "evo"
  def help: String = "evo [-v] <type> - Show evolution history. -v=verbose"

  def execute(args: Seq[String], ctx: ExploreContext[EitherF]): Either[String, String] = {
    val verbose  = args.contains("-v")
    val typeArgs = args.filterNot(_.startsWith("-"))

    typeArgs.headOption match {
      case None =>
        Left("Usage: evo [-v] <type>")

      case Some(typeName) =>
        ctx.currentLineage match {
          case None =>
            Left("No domain selected. Use 'switch <domain>' first.")
          case Some(lineage) =>
            ctx.findTypeId(typeName) match {
              case None =>
                Left(s"Type not found: $typeName")
              case Some(typeId) =>
                val sb = new StringBuilder
                sb.append(s"${Colors.CYAN}Evolution history for ${Colors.GREEN}$typeName${Colors.CYAN}:${Colors.RESET}\n\n")

                val sortedSteps = lineage.evolution.diffs.keys.toSeq.sortBy(_.from)

                if (sortedSteps.isEmpty) {
                  sb.append(s"${Colors.DIM}No evolution steps recorded${Colors.RESET}")
                } else {
                  sortedSteps.foreach {
                    step =>
                      val diff    = lineage.evolution.diffs(step)
                      val changes = diff.changes

                      val status = if (changes.added.contains(typeId)) {
                        s"${Colors.GREEN}+ Added${Colors.RESET}"
                      } else if (changes.removed.contains(typeId)) {
                        s"${Colors.RED}- Removed${Colors.RESET}"
                      } else if (changes.fullyModified.contains(typeId)) {
                        s"${Colors.YELLOW}~ Modified (full)${Colors.RESET}"
                      } else if (changes.shallowModified.contains(typeId)) {
                        s"${Colors.YELLOW}~ Modified (shallow)${Colors.RESET}"
                      } else if (changes.deepModified.contains(typeId)) {
                        s"${Colors.CYAN}~ Modified (deep deps)${Colors.RESET}"
                      } else if (changes.renamed.values.toSet.contains(typeId.asInstanceOf[TypeId.User])) {
                        val oldName = changes.renamed.find(_._2 == typeId).map(_._1.name.name).getOrElse("?")
                        s"${Colors.MAGENTA}> Renamed from $oldName${Colors.RESET}"
                      } else if (changes.renamed.contains(typeId.asInstanceOf[TypeId.User])) {
                        val newName = changes.renamed(typeId.asInstanceOf[TypeId.User]).name.name
                        s"${Colors.MAGENTA}> Renamed to $newName${Colors.RESET}"
                      } else if (changes.unmodified.contains(typeId)) {
                        s"${Colors.DIM}= Unchanged${Colors.RESET}"
                      } else {
                        s"${Colors.DIM}? Unknown${Colors.RESET}"
                      }

                      sb.append(s"  ${Colors.BLUE}${step.from}${Colors.RESET} -> ${Colors.BLUE}${step.to}${Colors.RESET}: $status\n")

                      if (verbose) {
                        diff.diffs.get(typeId).foreach {
                          typeDiff =>
                            typeDiff.ops.foreach {
                              op =>
                                sb.append(s"      ${formatOp(op)}\n")
                            }
                        }
                      }

                      // Show conversion info for this step
                      lineage.evolution.rules.get(step).foreach {
                        ruleset =>
                          ruleset.conversions.find(_.sourceTpe == typeId).foreach {
                            conv =>
                              sb.append(s"      ${formatConversionShort(conv)}\n")
                          }
                      }
                  }
                }

                lineage.evolution.typesUnchangedSince.get(ctx.currentVersion.get).flatMap(_.get(typeId)).foreach {
                  unmodified =>
                    sb.append(s"\n${Colors.GREEN}Unchanged since: ${unmodified.in}${Colors.RESET}")
                }

                Right(sb.toString().stripSuffix("\n"))
            }
        }
    }
  }

  // Op-formatting is delegated to the shared, color-optional renderer so there
  // is one source of truth. `useColor=true` reproduces the explorer's palette.
  private val opFormatter = new OpDiffFormatter(useColor = true)

  private def formatOp(op: AbstractOp): String = opFormatter.formatOp(op)

  private def formatConversionShort(conv: Conversion): String = conv match {
    case CustomConversionRequired(_, reason, _) =>
      val reasonStr = reason match {
        case DerivationFailure.IncompatibleFields(changes, additions) =>
          val parts = Seq(
            if (changes.nonEmpty) Some(s"incompatible: ${changes.map(c => c.f.name.name).mkString(", ")}") else None,
            if (additions.nonEmpty) Some(s"new required: ${additions.map(a => a.f.name.name).mkString(", ")}") else None,
          ).flatten
          parts.mkString("; ")
        case DerivationFailure.AdtBranchRemoved(ops) =>
          s"removed branches: ${ops.map(_.id.name.name).mkString(", ")}"
        case DerivationFailure.EnumBranchRemoved(ops) =>
          s"removed members: ${ops.map(_.m.name).mkString(", ")}"
        case DerivationFailure.IncompatibleRenames(renames) =>
          s"incompatible renames: ${renames.map(r => s"${r.oldField.name.name}->${r.newField.name.name}").mkString(", ")}"
        case DerivationFailure.Foreign =>
          "foreign type"
      }
      s"${Colors.RED}Conversion: manual${Colors.RESET} ($reasonStr)"
    case _: DtoConversion | _: CopyEnumByName | _: CopyAdtBranchByName =>
      s"${Colors.GREEN}Conversion: automatic${Colors.RESET}"
    case _: RemovedTypeNoConversion =>
      s"${Colors.DIM}Conversion: none (type removed)${Colors.RESET}"
    case _: NonDataTypeTypeNoConversion =>
      s"${Colors.DIM}Conversion: none (non-data type)${Colors.RESET}"
    case _ =>
      s"${Colors.DIM}Conversion: unknown${Colors.RESET}"
  }

  def complete(args: Seq[String], ctx: ExploreContext[EitherF]): Seq[String] = {
    val nonFlags = args.filterNot(_.startsWith("-"))
    val lastArg  = args.lastOption.getOrElse("")

    if (lastArg.startsWith("-")) {
      Seq("-v").filter(_.startsWith(lastArg))
    } else if (nonFlags.size <= 1) {
      ctx.allTypeIds
        .map(_.name.name)
        .filter(_.toLowerCase.contains(lastArg.toLowerCase))
    } else {
      Seq.empty
    }
  }
}
