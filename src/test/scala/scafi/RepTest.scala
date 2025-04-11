package scafi

import org.scalatest.matchers.should.Matchers.*
import AggregateFramework.*
import org.scalatest.Ignore

class RepTest extends org.scalatest.funsuite.AnyFunSuite:
  given dom: Set[Device] = Set(selfDevice)
  import AggregateFramework.Aggregate.given

  test("Constant rep"):
    val k = constant(5)
    k.eval(Empty()) shouldBe Rep(5, Empty())
    k.eval(Rep(5, Empty())) shouldBe Rep(5, Val(5))
    k.eval(Rep(5, Empty()))(using Set()) shouldBe Rep(5, Empty())
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

  test("Call to a non-aggregate program"):
    val f = () => compute(1)
    val c = aggregateCall(f)
    c.eval(Empty()) shouldBe Call(f, Val(1))

  test("Call to an aggregate program"):
    val f = () => counter(0)
    val c = aggregateCall(f)
    c.eval(Empty()) shouldBe Call(f, Rep(0, Empty()))
    c.eval(Call(f, Rep(0, Empty()))) shouldBe Call(f, Rep(1, Val(1)))

  test("Rep restarts in case of a synthetic function change"):
    val f1 = () => counter(0)
    val f2 = () => counter(0)
    val c1 = aggregateCall(f1)
    val c2 = aggregateCall(f2)
    c1.eval(Call(f1, Rep(0, Empty()))) shouldBe Call(f1, Rep(1, Val(1)))
    c2.eval(Call(f1, Rep(0, Empty()))) shouldBe Call(f2, Rep(0, Empty()))

  test("Branching two reps"):
    import AdditionalConstructs.*
    val b = branch(true)(counter(0))(counter(100))
    val b2 = branch(false)(counter(0))(counter(100))
    val Call(f, n1) = b.eval(Empty()); n1 shouldBe Rep(0, Empty())
    val Call(`f`, n2) = b.eval(Call(f, n1)); n2 shouldBe Rep(1, Val(1))
    // switching branch
    val Call(g, n3) = b2.eval(Call(f, n2)); n3 shouldBe Rep(100, Empty())
    val Call(`g`, n4) = b2.eval(Call(g, n3)); n4 shouldBe Rep(101, Val(101))
    // switching branch, rep restarted
    val Call(`f`, n5) = b.eval(Call(g, n4)); n5 shouldBe Rep(0, Empty())

