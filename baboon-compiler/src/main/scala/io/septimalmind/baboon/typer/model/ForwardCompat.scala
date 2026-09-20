package io.septimalmind.baboon.typer.model

import izumi.fundamentals.collections.nonempty.NEList

/** How much of a NEWER version's UEBA encoding an OLDER codec consumes correctly.
  *
  * UEBA is positional and name-blind: field, enum-member and ADT-branch identities
  * are indices, never strings. A declared rename therefore leaves the bytes
  * untouched, while a reordering or a mid-position insert shifts every following
  * value. This is the mirror image of JSON, where the name IS the identity.
  */
sealed abstract class UebaRead(val weight: Int)

object UebaRead {

  /** The old decoder consumes the whole value: the UEBA encoding is byte-identical. */
  case object Full extends UebaRead(3)

  /** The old fields are a positional prefix of the new ones and every appended
    * field is fixed-length, so compact and indexed blobs both prefix-read.
    */
  case object PrefixAnyMode extends UebaRead(2)

  /** The old fields are a positional prefix of the new ones but some appended
    * field is variable-length, so only compact blobs prefix-read: an indexed
    * blob carries an extra index entry and the index has no on-wire count.
    */
  case object PrefixCompact extends UebaRead(1)

  implicit val ordering: Ordering[UebaRead] = Ordering.by(_.weight)

  def min(a: UebaRead, b: UebaRead): UebaRead = if (a.weight <= b.weight) a else b
}

/** Forward-readability of one evolution step, or of a chain of them, resolved
  * independently per wire format.
  *
  * The two formats forgive different things and neither subsumes the other, so a
  * single linear scale cannot describe a step. A declared rename is `ueba = Full`
  * with `json = false`; a reorder or mid-position insert is the exact opposite.
  * Chains compose by [[meet]], which weakens each axis on its own.
  */
final case class ForwardGuarantee(ueba: Option[UebaRead], json: Boolean) {

  def isEmpty: Boolean = ueba.isEmpty && !json

  def nonEmpty: Boolean = !isEmpty

  /** Byte-identical in BOTH formats. `ueba = Full` fixes the field count and the
    * positional types, and `json` additionally forbids every rename, so the two
    * together can only hold when the member lists are equal.
    */
  def identical: Boolean = ueba.contains(UebaRead.Full) && json

  def meet(o: ForwardGuarantee): ForwardGuarantee =
    ForwardGuarantee(
      ueba = for { a <- ueba; b <- o.ueba } yield UebaRead.min(a, b),
      json = json && o.json,
    )

  /** Whether this guarantee is strong enough to publish `cap` as a bound. */
  def grants(cap: ForwardCompatTier): Boolean = cap match {
    case ForwardCompatTier.Identical     => identical
    case ForwardCompatTier.PrefixAnyMode => ueba.exists(_.weight >= UebaRead.PrefixAnyMode.weight)
    case ForwardCompatTier.PrefixCompact => ueba.nonEmpty
    case ForwardCompatTier.JsonAdditive  => json
  }

  /** Monotonically non-increasing under [[meet]]; used for ordering and invariants. */
  def weight: Int = ueba.fold(0)(_.weight) * 2 + (if (json) 1 else 0)

  /** Published in `baboonForwardReadable` and in the `forwardReadable` block of
    * `baboon-meta.json`. A `ueba-` prefix means the guarantee holds for UEBA only
    * and that the JSON encoding of this step is NOT readable by the older codec.
    */
  def wireName: String = (ueba, json) match {
    case (Some(UebaRead.Full), true)           => "identical"
    case (Some(UebaRead.Full), false)          => "ueba-identical"
    case (Some(UebaRead.PrefixAnyMode), true)  => "prefix-any-mode"
    case (Some(UebaRead.PrefixAnyMode), false) => "ueba-prefix-any-mode"
    case (Some(UebaRead.PrefixCompact), true)  => "prefix-compact"
    case (Some(UebaRead.PrefixCompact), false) => "ueba-prefix-compact"
    case (None, true)                          => "json-additive"
    case (None, false)                         => "none"
  }
}

object ForwardGuarantee {
  val identical: ForwardGuarantee = ForwardGuarantee(Some(UebaRead.Full), json = true)
  val none: ForwardGuarantee      = ForwardGuarantee(None, json = false)
  val jsonOnly: ForwardGuarantee  = ForwardGuarantee(None, json = true)
}

/** A capability a writer can publish a lower bound for, in `baboonMinReaderVersions`.
  *
  * These keys are already per-format and the runtimes consume them that way: the
  * binary envelope asks for `prefix-compact` or `prefix-any-mode` depending on the
  * payload's index mode, the JSON envelope asks for `json-additive`. None of them
  * implies any of the others.
  */
sealed abstract class ForwardCompatTier(val weight: Int, val wireName: String)

object ForwardCompatTier {

  /** Byte-identical in both formats; equals the type's `sameIn` head. */
  case object Identical extends ForwardCompatTier(4, "identical")

  /** UEBA: the old decoder reads compact AND indexed blobs. Says nothing about JSON. */
  case object PrefixAnyMode extends ForwardCompatTier(3, "prefix-any-mode")

  /** UEBA: the old decoder reads compact blobs. Says nothing about JSON. */
  case object PrefixCompact extends ForwardCompatTier(2, "prefix-compact")

  /** JSON: the old decoder reads the payload, at any field position. Says nothing about UEBA. */
  case object JsonAdditive extends ForwardCompatTier(1, "json-additive")

  implicit val ordering: Ordering[ForwardCompatTier] = Ordering.by(_.weight)

  val all: List[ForwardCompatTier] = List(Identical, PrefixAnyMode, PrefixCompact, JsonAdditive)
}

/** For a type as seen by version `in`: the ascending contiguous list of versions
  * whose blobs the `in`-version codec can read, with the guarantee for each.
  * Invariants: `readable.head == (in, ForwardGuarantee.identical)`; versions
  * strictly ascending; guarantees non-increasing on both axes.
  */
case class ForwardReadable(typeId: TypeId, in: Version, readable: NEList[(Version, ForwardGuarantee)]) {
  def tierFor(version: Version): Option[ForwardGuarantee] =
    readable.toList.collectFirst { case (v, t) if v == version => t }
}
