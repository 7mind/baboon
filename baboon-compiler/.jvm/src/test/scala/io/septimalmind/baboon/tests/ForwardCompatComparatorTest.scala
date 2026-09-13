package io.septimalmind.baboon.tests

import io.septimalmind.baboon.BaboonLoader
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.Error2
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.files.IzFiles
import izumi.fundamentals.platform.resources.IzResources
import izumi.reflect.TagKK

final class ForwardCompatComparatorTest extends ForwardCompatComparatorTestBase[Either]

abstract class ForwardCompatComparatorTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {
  import ForwardCompatTier.*

  private def loadFwd(loader: BaboonLoader[F]): F[NEList[BaboonIssue], BaboonFamily] = {
    val root = IzResources
      .getPath("fwd-compat-ok")
      .get
      .asInstanceOf[IzResources.LoadablePathReference]
      .path
    val baboons = IzFiles
      .walk(root.toFile)
      .toList
      .filter(p => p.toFile.isFile && p.toFile.getName.endsWith(".baboon"))
    loader.load(baboons)
  }

  private def evolutionOf(family: BaboonFamily): BaboonEvolution = {
    family.domains.toMap.values.find(_.pkg.toString == "fwdcompat.model").get.evolution
  }

  private def runOf(evo: BaboonEvolution, version: String, typeName: String): List[(String, ForwardCompatTier)] = {
    val entries = evo.typesForwardReadable(Version.parse(version)).collect {
      case (id: TypeId.User, fr) if id.name.name == typeName => fr
    }.toList
    assert(entries.size == 1, s"expected exactly one type named $typeName in $version, got ${entries.size}")
    entries.head.readable.toList.map { case (v, t) => (v.toString, t) }
  }

  "forward-compat comparator" should {
    "classify per-type forward-readability runs across the version chain" in {
      (loader: BaboonLoader[F]) =>
        for {
          family <- loadFwd(loader)
        } yield {
          val evo = evolutionOf(family)

          // fixed-length append: readable in both UEBA modes (top-level framed reads)
          assert(runOf(evo, "1.0.0", "AppendFixed") == List(("1.0.0", Identical), ("1.1.0", PrefixAnyMode), ("1.2.0", PrefixAnyMode)))

          // variable-length appends: compact blobs only, composed across two steps
          assert(runOf(evo, "1.0.0", "AppendVar") == List(("1.0.0", Identical), ("1.1.0", PrefixCompact), ("1.2.0", PrefixCompact)))
          assert(runOf(evo, "1.1.0", "AppendVar") == List(("1.1.0", Identical), ("1.2.0", PrefixCompact)))

          // mid-position insertion: JSON only; run ends when a field is removed in 1.2.0
          assert(runOf(evo, "1.0.0", "MidInsert") == List(("1.0.0", Identical), ("1.1.0", JsonAdditive)))
          assert(runOf(evo, "1.1.0", "MidInsert") == List(("1.1.0", Identical)))

          // field removal / field type change: not forward-readable
          assert(runOf(evo, "1.0.0", "Removed") == List(("1.0.0", Identical)))
          assert(runOf(evo, "1.0.0", "Changed") == List(("1.0.0", Identical)))

          // own structure unchanged but a dependency appends: capped at JSON
          assert(runOf(evo, "1.0.0", "HostOfAppend") == List(("1.0.0", Identical), ("1.1.0", JsonAdditive), ("1.2.0", JsonAdditive)))

          // fully unchanged closure: byte-identical throughout
          assert(runOf(evo, "1.0.0", "HostOfUnchanged") == List(("1.0.0", Identical), ("1.1.0", Identical), ("1.2.0", Identical)))
          assert(runOf(evo, "1.0.0", "Stable") == List(("1.0.0", Identical), ("1.1.0", Identical), ("1.2.0", Identical)))

          // enum gains a member: unreadable (new values would throw), host follows
          assert(runOf(evo, "1.0.0", "EnumGrows") == List(("1.0.0", Identical)))
          assert(runOf(evo, "1.0.0", "EnumGrowsHost") == List(("1.0.0", Identical)))

          // enum members reordered: JSON encodes names, UEBA discriminants are positional
          assert(runOf(evo, "1.0.0", "EnumReorder") == List(("1.0.0", Identical), ("1.1.0", JsonAdditive), ("1.2.0", JsonAdditive)))
          assert(runOf(evo, "1.0.0", "EnumReorderHost") == List(("1.0.0", Identical), ("1.1.0", JsonAdditive), ("1.2.0", JsonAdditive)))

          // recursive DTO appending a field: self-dependency demotes PREFIX to JSON
          assert(runOf(evo, "1.0.0", "Recur") == List(("1.0.0", Identical), ("1.1.0", JsonAdditive), ("1.2.0", JsonAdditive)))

          // ADT branch appends a field: the branch itself is prefix-readable,
          // the ADT (whose decode nests the branch) is capped at JSON
          assert(runOf(evo, "1.0.0", "L") == List(("1.0.0", Identical), ("1.1.0", PrefixCompact), ("1.2.0", PrefixCompact)))
          assert(runOf(evo, "1.0.0", "Sum") == List(("1.0.0", Identical), ("1.1.0", JsonAdditive), ("1.2.0", JsonAdditive)))
        }
    }

    "maintain structural invariants and consistency with sameIn ranges" in {
      (loader: BaboonLoader[F]) =>
        for {
          family <- loadFwd(loader)
        } yield {
          val evo = evolutionOf(family)

          evo.typesForwardReadable.foreach {
            case (version, types) =>
              types.foreach {
                case (id, fr) =>
                  assert(fr.typeId == id)
                  assert(fr.in == version)
                  // head is always the own version at IDENTICAL
                  assert(fr.readable.head == ((version, Identical)))
                  // versions strictly ascending
                  val vs = fr.readable.toList.map(_._1)
                  assert(vs == vs.sorted(Version.ordering) && vs.distinct == vs)
                  // tiers non-increasing
                  val weights = fr.readable.toList.map(_._2.weight)
                  assert(weights.zip(weights.tail).forall { case (a, b) => a >= b })
              }
          }

          // Soundness direction: an IDENTICAL forward tier implies byte-identity,
          // so the version must be a sameIn twin. The converse does NOT hold:
          // `deepSchemaRepr` sorts the flattened dependency representation lines
          // per field (BaboonTyper), erasing member/field ORDER inside
          // dependencies — so sameIn overclaims byte-identity when a dependency
          // was merely reordered (see EnumReorderHost below).
          evo.typesForwardReadable.foreach {
            case (version, types) =>
              types.foreach {
                case (id, fr) =>
                  val twins = evo.typesUnchangedSince(version)(id).higherTwins(version).toSet
                  fr.readable.toList.drop(1).foreach {
                    case (v, tier) =>
                      if (tier == Identical) {
                        assert(twins.contains(v), s"$id@$version: forward-Identical $v must be a sameIn twin")
                      }
                  }
              }
          }

          // Regression documentation of the known sameIn overclaim: the host of a
          // reordered enum stays `unmodified` (order-erased deepId) although its
          // UEBA bytes change; the forward metadata correctly demotes it to JSON.
          val reorderHostId = evo.typesForwardReadable(Version.parse("1.0.0")).keys.collectFirst {
            case id: TypeId.User if id.name.name == "EnumReorderHost" => id: TypeId
          }.get
          val reorderHostTwins = evo.typesUnchangedSince(Version.parse("1.0.0"))(reorderHostId).higherTwins(Version.parse("1.0.0"))
          assert(reorderHostTwins.contains(Version.parse("1.1.0")), "precondition: sameIn still overclaims for EnumReorderHost")
          assert(runOf(evo, "1.0.0", "EnumReorderHost").tail.forall(_._2 == JsonAdditive))
        }
    }
  }
}
