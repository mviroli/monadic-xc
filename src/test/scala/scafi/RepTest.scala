package scafi

import org.scalatest.matchers.should.Matchers.*
import AggregateFramework.*

class RepTest extends org.scalatest.funsuite.AnyFunSuite:

  test("Constant rep"):
    val k = constant(5)
    k.eval(Empty()) shouldBe Rep(5, Empty())
    k.eval(Rep(5, Empty())) shouldBe Rep(5, Val(5))
    k.eval(Rep(5, Val(5))) shouldBe Rep(5, Val(5))

  test("Constant counter"):
    val c = counter(0)
    c.eval(Empty()) shouldBe Rep(0, Empty())
    c.eval(Rep(4, Empty())) shouldBe Rep(5, Val(5))
    c.eval(Rep(5, Val(5))) shouldBe Rep(6, Val(6))

  test("Counter with nesting"):
    val c = counterWithNesting(0)
    c.eval(Empty()) shouldBe Rep(0, Empty())
    c.eval(Rep(0, Empty())) shouldBe Rep(1, Next(Rep(1, Empty()), Val(1)))
    c.eval(Rep(1, Next(Rep(1, Empty()), Val(1)))) shouldBe Rep(2, Next(Rep(1, Val(1)), Val(2)))