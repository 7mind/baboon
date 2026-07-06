package io.septimalmind.baboon.bincompat

import io.circe.Json
import io.septimalmind.baboon.typer.model.{DerivationFailure, TypeId}

/** Pure renderer for [[BincompatResult]] — the structured output of
  * [[BincompatClassifier.classify]].
  *
  * Two output modes:
  *
  *   - [[renderText]] — human-readable, suitable for terminal output. Exit-code
  *     semantics are embedded in the messages (no I/O is performed here).
  *   - [[renderJson]] — machine-readable JSON with the shape
  *     `{exitCode, derivable[], nonDerivable[{type,reason}]}`.
  *
  * Both methods are pure functions of [[BincompatResult]]; they perform no I/O
  * and have no side effects.
  *
  * Type ids are rendered via `TypeId.User#render` (dot-separated fully-qualified
  * name), consistent with [[io.septimalmind.baboon.diff.BaboonDiffRenderer]].
  */
object BincompatRenderer {

  /** Human-readable text rendering of a [[BincompatResult]].
    *
    * Exit 0 ([[BincompatVerdict.NoBreak]]): a single-line no-breaking-changes
    * message.
    *
    * Exit 1 ([[BincompatVerdict.BreakingDerivable]]): a header message followed
    * by a grouped list of derivable changes (one per type, annotated with its
    * kind).
    *
    * Exit 2 ([[BincompatVerdict.NonDerivable]]): an explanation message listing
    * each non-derivable change annotated with its [[NonDerivableChange.Reason]].
    */
  def renderText(result: BincompatResult): String = result.verdict match {
    case BincompatVerdict.NoBreak =>
      "No breaking changes detected."

    case BincompatVerdict.BreakingDerivable =>
      val header = "Breaking changes detected, but all are automatically derivable (exit 1)."
      val lines  = result.derivable.sortBy(c => typeLabel(c.sourceTpe)).map(renderDerivableText)
      if (lines.isEmpty) header
      else s"$header\n\nDerivable changes:\n${lines.map(l => s"  $l").mkString("\n")}"

    case BincompatVerdict.NonDerivable =>
      val header = "Non-derivable breaking changes detected (exit 2). Manual conversions required."
      val lines  = result.nonDerivable.sortBy(c => typeLabel(c.sourceTpe)).map(renderNonDerivableText)
      if (lines.isEmpty) header
      else s"$header\n\nNon-derivable changes:\n${lines.map(l => s"  $l").mkString("\n")}"
  }

  /** Machine-readable JSON rendering of a [[BincompatResult]].
    *
    * Produces a JSON object with the following shape:
    * {{{
    * {
    *   "exitCode": <0|1|2>,
    *   "derivable": ["pkg.Type (kind)", ...],
    *   "nonDerivable": [
    *     { "type": "pkg.Type", "reason": "reason string" },
    *     ...
    *   ]
    * }
    * }}}
    *
    * The `nonDerivable` entries carry a `"reason"` field whose value is the
    * human-readable reason string for the [[NonDerivableChange.Reason]], so that
    * callers can parse the verdict beyond the exit code alone.
    */
  def renderJson(result: BincompatResult): String = {
    val exitCode = Json.fromInt(result.verdict.code)

    val derivableJson = Json.arr(
      result.derivable
        .sortBy(c => typeLabel(c.sourceTpe))
        .map(c => Json.fromString(renderDerivableText(c)))*
    )

    val nonDerivableJson = Json.arr(
      result.nonDerivable
        .sortBy(c => typeLabel(c.sourceTpe))
        .map { c =>
          Json.obj(
            "type"   -> Json.fromString(typeLabel(c.sourceTpe)),
            "reason" -> Json.fromString(renderReasonText(c.reason)),
          )
        }*
    )

    Json
      .obj(
        "exitCode"     -> exitCode,
        "derivable"    -> derivableJson,
        "nonDerivable" -> nonDerivableJson,
      )
      .spaces2
  }

  // -- private helpers --------------------------------------------------------

  private def typeLabel(id: TypeId.User): String = id.render

  private def renderDerivableText(c: DerivableChange): String = {
    val kind = c.kind match {
      case DerivableChange.Kind.ModifiedType    => "modified"
      case DerivableChange.Kind.DerivableRemoval => "removed (no surviving references)"
    }
    s"${typeLabel(c.sourceTpe)} ($kind)"
  }

  private def renderNonDerivableText(c: NonDerivableChange): String =
    s"${typeLabel(c.sourceTpe)}: ${renderReasonText(c.reason)}"

  private def renderReasonText(reason: NonDerivableChange.Reason): String = reason match {
    case NonDerivableChange.Reason.RemovedTypeStillReferenced =>
      "removed but still referenced by a surviving type"
    case NonDerivableChange.Reason.CustomConversion(failure) =>
      renderDerivationFailure(failure)
  }

  private def renderDerivationFailure(f: DerivationFailure): String = f match {
    case DerivationFailure.Foreign =>
      "foreign type: automatic derivation is not supported for foreign types"
    case DerivationFailure.EnumBranchRemoved(ops) =>
      val names = ops.map(_.m.name).mkString(", ")
      s"enum branch removed: $names"
    case DerivationFailure.AdtBranchRemoved(ops) =>
      val names = ops.map(_.id.name.name).mkString(", ")
      s"ADT branch removed: $names"
    case DerivationFailure.IncompatibleFields(changes, additions) =>
      val changeNames = changes.map(_.f.name.name).toList.sorted.mkString(", ")
      val addNames    = additions.map(_.f.name.name).toList.sorted.mkString(", ")
      val parts = List(
        if (changes.nonEmpty) s"incompatible field changes: $changeNames" else "",
        if (additions.nonEmpty) s"incompatible field additions: $addNames" else "",
      ).filter(_.nonEmpty)
      parts.mkString("; ")
    case DerivationFailure.IncompatibleRenames(renames) =>
      val names = renames.map(r => s"${r.oldField.name.name} -> ${r.newField.name.name}").toList.sorted.mkString(", ")
      s"incompatible field renames: $names"
  }
}
