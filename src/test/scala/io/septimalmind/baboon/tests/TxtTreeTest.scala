package io.septimalmind.baboon.tests

import io.septimalmind.baboon.tests.TxtTreeTest.{TestVal, TestVal2}
import io.septimalmind.baboon.translator.TextTree.*
import org.scalatest.wordspec.AnyWordSpec

class TxtTreeTest extends AnyWordSpec {
  "TxtTree" should {
    "properly handle interpolations" in {
      assert(
        q"test1 ${TestVal("1")} test2 ${TestVal("2")} test3".render == "test1 <1> test2 <2> test3"
      )

      assert(q"${TestVal("1")} test2 ${TestVal("2")}".render == "<1> test2 <2>")

      assert(q"${TestVal("1")}".render == "<1>")

      assert((q"test": Node[Nothing]).render == "test")
      assert(q"test".render == "test")
      assert(q"".render == "")
    }

    "handle tree nesting" in {
      val t1 = q"${TestVal("1")}"
      val t2 = q"test"

      val t3 =
        q"t1: $t1, t2: $t2, t3: ${TestVal("3")}"

      assert(t3.render == "t1: <1>, t2: test, t3: <3>")
      assert(t3.render == t3.flatten.render)
      assert(t3.map(v => TestVal2(v.value)).render == t3.render)
    }
  }
}

object TxtTreeTest {
  case class TestVal(value: String)
  case class TestVal2(value: String)

  implicit val TestValTT: TTValue[TestVal] = new TTValue[TestVal] {
    override def render(value: TestVal): String = s"<${value.value}>"
  }

  implicit val TestVal2TT: TTValue[TestVal2] = new TTValue[TestVal2] {
    override def render(value: TestVal2): String = s"<${value.value}>"
  }

}
