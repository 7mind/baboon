package io.septimalmind.baboon.diff

import io.circe.Json
import io.septimalmind.baboon.typer.model.*

/** Whole-domain diff renderer built on top of the shared per-op formatter
  * ([[OpDiffFormatter]], T167).
  *
  * Layers two views of a [[BaboonDiff]]:
  *   1. a coarse type-set summary header derived from [[BaboonChanges]]
  *      (added / removed / renamed / shallow- / deep- / fully-modified /
  *      unmodified type-id sets), and
  *   2. a per-type expansion of every changed (and renamed) type's
  *      [[TypedefDiff]] ops, formatted through [[OpDiffFormatter]].
  *
  * Two output modes are provided:
  *   - [[renderText]] — human-readable, with the color flag threaded to the
  *     op-formatter (default), and
  *   - [[renderJson]] — a stable JSON object (type-set arrays + a per-type ops
  *     array) that round-trips through the circe parser.
  *
  * All type-id collections are sorted (by their fully-qualified [[TypeId]]
  * rendering) so both outputs are deterministic and test-stable.
  *
  * Lives in shared `src/main` (NOT under `.jvm`), like [[OpDiffFormatter]], so
  * it can be exposed to Scala.js. It therefore carries no dependency on
  * JVM-only `explore.*` sources.
  */
final class BaboonDiffRenderer {

  /** Human-readable whole-domain diff. `useColor` is threaded straight to the
    * op-formatter; when false the output is plain text.
    */
  def renderText(diff: BaboonDiff, useColor: Boolean = false): String = {
    val formatter = new OpDiffFormatter(useColor)
    val c         = diff.changes

    val header = List(
      s"Diff ${diff.id.from} -> ${diff.id.to}",
      "",
      renderSet("Added", c.added),
      renderSet("Removed", c.removed),
      renderRenamed(c.renamed),
      renderSet("Modified (shallow)", c.shallowModified),
      renderSet("Modified (deep)", c.deepModified),
      renderSet("Modified (full)", c.fullyModified),
      renderSet("Unmodified", c.unmodified),
    ).mkString("\n")

    // Per-type ops for every type that carries a TypedefDiff. Sorted by
    // fully-qualified id so the section order is stable.
    val perType = sortedIds(diff.diffs.keySet).flatMap { id =>
      diff.diffs.get(id).map { td =>
        val opsBlock =
          if (td.ops.isEmpty) "  (no ops)"
          else td.ops.map(op => s"  ${formatter.formatOp(op)}").mkString("\n")
        s"${typeIdLabel(id)}:\n$opsBlock"
      }
    }

    val perTypeSection =
      if (perType.isEmpty) ""
      else "\n\nPer-type changes:\n\n" + perType.mkString("\n\n")

    header + perTypeSection
  }

  /** Structured whole-domain diff, serialized with circe. The object has a
    * `from`/`to` version pair, a `changes` object with one sorted array per
    * type-set, and a `diffs` array of `{ type, ops }` entries (one per type
    * carrying a [[TypedefDiff]], sorted by type id).
    */
  def renderJson(diff: BaboonDiff): String = {
    val c = diff.changes

    val changesJson = Json.obj(
      "added"           -> idsToJson(c.added),
      "removed"         -> idsToJson(c.removed),
      "renamed"         -> renamedToJson(c.renamed),
      "shallowModified" -> idsToJson(c.shallowModified),
      "deepModified"    -> idsToJson(c.deepModified),
      "fullyModified"   -> idsToJson(c.fullyModified),
      "unmodified"      -> idsToJson(c.unmodified),
    )

    val plainFormatter = new OpDiffFormatter(useColor = false)

    val diffsJson = Json.arr(
      sortedIds(diff.diffs.keySet).flatMap { id =>
        diff.diffs.get(id).map { td =>
          Json.obj(
            "type" -> Json.fromString(typeIdLabel(id)),
            "kind" -> Json.fromString(diffKind(td)),
            "ops" -> Json.arr(
              td.ops.map(op => Json.fromString(plainFormatter.formatOp(op)))*
            ),
          )
        }
      }*
    )

    val root = Json.obj(
      "from"    -> Json.fromString(diff.id.from.toString),
      "to"      -> Json.fromString(diff.id.to.toString),
      "changes" -> changesJson,
      "diffs"   -> diffsJson,
    )

    root.spaces2
  }

  // -- helpers ---------------------------------------------------------------

  private def diffKind(td: TypedefDiff): String = td match {
    case _: TypedefDiff.EnumDiff     => "enum"
    case _: TypedefDiff.DtoDiff      => "dto"
    case _: TypedefDiff.AdtDiff      => "adt"
    case _: TypedefDiff.ServiceDiff  => "service"
    case _: TypedefDiff.ContractDiff => "contract"
  }

  /** Fully-qualified rendering used both as the stable sort key and as the
    * type label in output. `User` ids carry pkg/owner so labels are unique;
    * builtins fall back to their `toString`.
    */
  private def typeIdLabel(id: TypeId): String = id match {
    case u: TypeId.User => u.render
    case other          => other.toString
  }

  private def sortedIds(ids: Iterable[TypeId]): List[TypeId] =
    ids.toList.sortBy(typeIdLabel)

  private def renderSet(label: String, ids: Set[TypeId]): String = {
    val names = sortedIds(ids).map(typeIdLabel)
    if (names.isEmpty) s"$label: (none)"
    else s"$label:\n${names.map(n => s"  $n").mkString("\n")}"
  }

  private def renderRenamed(renamed: Map[TypeId.User, TypeId.User]): String = {
    val entries = renamed.toList
      .sortBy { case (from, _) => typeIdLabel(from) }
      .map { case (from, to) => s"  ${typeIdLabel(from)} -> ${typeIdLabel(to)}" }
    if (entries.isEmpty) "Renamed: (none)"
    else s"Renamed:\n${entries.mkString("\n")}"
  }

  private def idsToJson(ids: Set[TypeId]): Json =
    Json.arr(sortedIds(ids).map(id => Json.fromString(typeIdLabel(id)))*)

  private def renamedToJson(renamed: Map[TypeId.User, TypeId.User]): Json =
    Json.arr(
      renamed.toList
        .sortBy { case (from, _) => typeIdLabel(from) }
        .map {
          case (from, to) =>
            Json.obj(
              "from" -> Json.fromString(typeIdLabel(from)),
              "to"   -> Json.fromString(typeIdLabel(to)),
            )
        }*
    )
}
