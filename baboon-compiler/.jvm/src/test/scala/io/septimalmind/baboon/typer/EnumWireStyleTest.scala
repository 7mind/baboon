package io.septimalmind.baboon.typer

import org.scalatest.wordspec.AnyWordSpec

class EnumWireStyleTest extends AnyWordSpec {

  "EnumWireStyle.wireName" should {

    "capitalise a simple lowercase name" in {
      assert(EnumWireStyle.wireName("cafe") == "Cafe")
    }

    "capitalise only the first character, leaving underscores intact" in {
      // NOT BarPub — underscores are not word boundaries
      assert(EnumWireStyle.wireName("bar_pub") == "Bar_pub")
    }

    "leave an already-Pascal name unchanged" in {
      assert(EnumWireStyle.wireName("Already") == "Already")
    }

    "return an empty string unchanged" in {
      assert(EnumWireStyle.wireName("") == "")
    }

    "leave an underscore-leading name unchanged" in {
      // First char is '_', which .capitalize leaves as-is
      assert(EnumWireStyle.wireName("_foo") == "_foo")
    }

    "leave a digit-leading name unchanged" in {
      // First char is '1', which .capitalize leaves as-is
      assert(EnumWireStyle.wireName("1foo") == "1foo")
    }
  }
}
