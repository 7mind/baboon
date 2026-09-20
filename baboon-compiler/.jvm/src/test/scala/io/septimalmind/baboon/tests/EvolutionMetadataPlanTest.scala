package io.septimalmind.baboon.tests

import io.septimalmind.baboon.translator.EvolutionMetadataPlan
import io.septimalmind.baboon.translator.EvolutionMetadataPlan.{ReaderVersion, SameIn}
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.collections.nonempty.NEList
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EvolutionMetadataPlanTest extends AnyFlatSpec with Matchers {
  private val pkg   = Pkg(NEList("metadata"))
  private val typeA = TypeId.User(pkg, Owner.Toplevel, TypeName("A"))
  private val z     = TypeId.User(pkg, Owner.Toplevel, TypeName("Z"))
  private val v1    = Version.parse("1.0.0")
  private val v2    = Version.parse("2.0.0")
  private val v10   = Version.parse("10.0.0")
  private val evolution = BaboonEvolution(
    pkg,
    v10,
    Map.empty,
    Map.empty,
    Map(v2 -> Map(
      z                   -> UnmodifiedSince(z, v2, NEList(v1, v2, v10)),
      typeA               -> UnmodifiedSince(typeA, v2, NEList(v2)),
      TypeId.Builtins.str -> UnmodifiedSince(TypeId.Builtins.str, v2, NEList(v1, v2)),
    )),
    Map(v2 -> Map(
      z     -> ForwardReadable(z, v2, NEList(v2 -> ForwardGuarantee.identical, v10 -> ForwardGuarantee.jsonOnly)),
      typeA -> ForwardReadable(typeA, v2, NEList(v2 -> ForwardGuarantee.identical)),
    )),
  )

  "Evolution metadata preparation" should "sort type rows while preserving version order and builtin rows" in {
    val plan = EvolutionMetadataPlan(evolution, v2)
    plan.sameIn shouldBe List(
      SameIn(TypeId.Builtins.str, List("1.0.0", "2.0.0")),
      SameIn(typeA, List("2.0.0")),
      SameIn(z, List("1.0.0", "2.0.0", "10.0.0")),
    )
    plan.forwardReadable.map(_.typeId) shouldBe List(typeA, z)
    plan.forwardReadable.last.versions shouldBe List(
      ReaderVersion("2.0.0", "identical"),
      ReaderVersion("10.0.0", "json-additive"),
    )
  }

  it should "retain sparse tables without synthesizing missing type entries" in {
    val plan = EvolutionMetadataPlan(evolution, v2)
    plan.forwardReadable.exists(_.typeId == TypeId.Builtins.str) shouldBe false
    val empty = evolution.copy(typesUnchangedSince = Map(v2 -> Map.empty), typesForwardReadable = Map(v2 -> Map.empty))
    EvolutionMetadataPlan(empty, v2) shouldBe EvolutionMetadataPlan(Nil, Nil)
  }

  it should "fail when the requested version is absent instead of creating empty metadata" in {
    intercept[NoSuchElementException](EvolutionMetadataPlan(evolution, v1))
    intercept[NoSuchElementException](EvolutionMetadataPlan(evolution.copy(typesForwardReadable = Map.empty), v2))
  }
}
