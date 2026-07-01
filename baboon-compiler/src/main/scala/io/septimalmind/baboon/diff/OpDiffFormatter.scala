package io.septimalmind.baboon.diff

import io.septimalmind.baboon.typer.model.*

/** Color-optional, per-op diff renderer shared across platforms.
  *
  * Lives in shared `src/main` (NOT under `.jvm`) so it can be exposed to
  * Scala.js cheaply. It therefore MUST NOT reference `explore.Colors`, which is
  * JVM-only. The ANSI palette is inlined below as [[OpDiffFormatter.DiffColors]]
  * (identical escape strings to the explorer's `Colors`) and every escape is
  * gated on the `useColor` flag: when false, each token renders as the empty
  * string, yielding plain text.
  */
final class OpDiffFormatter(useColor: Boolean) {
  import OpDiffFormatter.DiffColors

  private def reset: String   = if (useColor) DiffColors.RESET else ""
  private def red: String     = if (useColor) DiffColors.RED else ""
  private def green: String   = if (useColor) DiffColors.GREEN else ""
  private def yellow: String  = if (useColor) DiffColors.YELLOW else ""
  private def magenta: String = if (useColor) DiffColors.MAGENTA else ""
  private def cyan: String    = if (useColor) DiffColors.CYAN else ""
  private def dim: String     = if (useColor) DiffColors.DIM else ""

  def formatOp(op: AbstractOp): String = op match {
    case DtoOp.AddField(f) =>
      s"$green+ ${f.name.name}$reset: ${formatTypeRef(f.tpe)}"
    case DtoOp.RemoveField(f) =>
      s"$red- ${f.name.name}$reset: ${formatTypeRef(f.tpe)}"
    case DtoOp.ChangeField(f, newType) =>
      s"$yellow~ ${f.name.name}$reset: ${formatTypeRef(f.tpe)} -> ${formatTypeRef(newType)}"
    case DtoOp.KeepField(f, modification) =>
      s"$dim= ${f.name.name}$reset: ${formatTypeRef(f.tpe)} (${formatModification(modification)})"
    case DtoOp.RenameField(oldField, newField, modification) =>
      val typeChangeStr = if (oldField.tpe != newField.tpe) {
        s" (${formatTypeRef(oldField.tpe)} -> ${formatTypeRef(newField.tpe)})"
      } else {
        s": ${formatTypeRef(newField.tpe)}"
      }
      s"$magenta* ${oldField.name.name} -> ${newField.name.name}$reset$typeChangeStr (${formatModification(modification)})"

    case EnumOp.AddBranch(m) =>
      s"$green+ ${m.name}$reset"
    case EnumOp.RemoveBranch(m) =>
      s"$red- ${m.name}$reset"
    case EnumOp.KeepBranch(m) =>
      s"$dim= ${m.name}$reset"

    case AdtOp.AddBranch(id) =>
      s"$green+ branch ${id.name.name}$reset"
    case AdtOp.RemoveBranch(id) =>
      s"$red- branch ${id.name.name}$reset"
    case AdtOp.KeepBranch(id, modification) =>
      s"$dim= branch ${id.name.name}$reset (${formatModification(modification)})"

    case ServiceOp.AddMethod(m) =>
      s"$green+ method ${m.name.name}$reset"
    case ServiceOp.RemoveMethod(m) =>
      s"$red- method ${m.name.name}$reset"
    case ServiceOp.KeepMethod(m) =>
      s"$dim= method ${m.name.name}$reset"

    case ContractOp.AddField(f) =>
      s"$green+ ${f.name.name}$reset: ${formatTypeRef(f.tpe)}"
    case ContractOp.RemoveField(f) =>
      s"$red- ${f.name.name}$reset: ${formatTypeRef(f.tpe)}"
    case ContractOp.KeepField(f) =>
      s"$dim= ${f.name.name}$reset: ${formatTypeRef(f.tpe)}"
    case ContractOp.AddContract(id) =>
      s"$green+ contract ${id.name.name}$reset"
    case ContractOp.RemoveContract(id) =>
      s"$red- contract ${id.name.name}$reset"
    case ContractOp.KeepContract(id) =>
      s"$dim= contract ${id.name.name}$reset"
  }

  private def formatModification(modification: RefModification): String = modification match {
    case RefModification.Unchanged => s"${dim}unchanged$reset"
    case RefModification.Shallow   => s"${cyan}shallow change$reset"
    case RefModification.Deep      => s"${cyan}deep change$reset"
    case RefModification.Full      => s"${yellow}full change$reset"
  }

  private def formatTypeRef(ref: TypeRef): String = ref match {
    case TypeRef.Scalar(id) => formatTypeId(id)
    case TypeRef.Constructor(id, args) =>
      s"$cyan${id.name.name}$reset[${args.toList.map(formatTypeRef).mkString(", ")}]"
    case _: TypeRef.Any => AnyPlaceholder.notSupportedYet("OpDiffFormatter.formatTypeRef")
  }

  private def formatTypeId(id: TypeId): String = id match {
    case b: TypeId.Builtin => s"$cyan${b.name.name}$reset"
    case u: TypeId.User    => s"$green${u.name.name}$reset"
  }
}

object OpDiffFormatter {

  /** ANSI palette inlined from the JVM-only `explore.Colors` so the shared
    * formatter carries no dependency on `.jvm` sources. Escape strings are
    * identical to `Colors`. `useColor=false` bypasses this object entirely.
    */
  private object DiffColors {
    val RESET   = "\u001b[0m"
    val RED     = "\u001b[31m"
    val GREEN   = "\u001b[32m"
    val YELLOW  = "\u001b[33m"
    val BLUE    = "\u001b[34m"
    val MAGENTA = "\u001b[35m"
    val CYAN    = "\u001b[36m"
    val WHITE   = "\u001b[37m"
    val BOLD    = "\u001b[1m"
    val DIM     = "\u001b[2m"
  }
}
