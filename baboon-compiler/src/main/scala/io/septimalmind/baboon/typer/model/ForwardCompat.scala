package io.septimalmind.baboon.typer.model

import izumi.fundamentals.collections.nonempty.NEList

/** Forward-readability guarantee: whether a codec generated for an OLDER version
  * can decode data produced by a NEWER version's codec, for any value the newer
  * version can emit. Tiers are linearly ordered by strength; each tier implies
  * every guarantee of the tiers below it.
  *
  * See docs/drafts/20260911-0937-forward-compat-metadata.md for the design and
  * the soundness argument.
  */
sealed abstract class ForwardCompatTier(val weight: Int, val wireName: String)

object ForwardCompatTier {
  /** Byte-identical encoding: both wire formats, any nesting position. */
  case object Identical extends ForwardCompatTier(4, "identical")

  /** UEBA: prefix-read of compact AND indexed blobs (all appended fields are
    * fixed-length, so the index shape is unchanged); sound only for top-level
    * framed reads where the caller discards the cursor. JSON: any position.
    */
  case object PrefixAnyMode extends ForwardCompatTier(3, "prefix-any-mode")

  /** UEBA: prefix-read of compact blobs only (some appended field is
    * variable-length; an indexed blob would desync the reader's index parse);
    * top-level framed reads only. JSON: any position.
    */
  case object PrefixCompact extends ForwardCompatTier(2, "prefix-compact")

  /** JSON only (tolerant readers ignore unknown keys), any nesting position. */
  case object JsonAdditive extends ForwardCompatTier(1, "json-additive")

  implicit val ordering: Ordering[ForwardCompatTier] = Ordering.by(_.weight)

  def min(a: ForwardCompatTier, b: ForwardCompatTier): ForwardCompatTier =
    if (a.weight <= b.weight) a else b

  val all: List[ForwardCompatTier] = List(Identical, PrefixAnyMode, PrefixCompact, JsonAdditive)
}

/** For a type as seen by version `in`: the ascending contiguous list of versions
  * whose blobs the `in`-version codec can read, with the strongest guarantee tier
  * for each. Invariants: `readable.head == (in, Identical)`; versions strictly
  * ascending; tiers non-increasing.
  */
case class ForwardReadable(typeId: TypeId, in: Version, readable: NEList[(Version, ForwardCompatTier)]) {
  def tierFor(version: Version): Option[ForwardCompatTier] =
    readable.toList.collectFirst { case (v, t) if v == version => t }
}
