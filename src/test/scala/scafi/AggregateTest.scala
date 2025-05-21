package scafi

import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers.*
import scafi.AggregateTestUtilities.{*, given}
import scafi.Aggregates.{*, given}
import NValues.NValueInternal.*

class AggregateTest extends org.scalatest.funsuite.AnyFunSuite:
  import Tree.*
  def counter(initial: Int) = rep(initial)(for i <- _ yield i + 1)
  given Device = selfDevice

  test("constant"):
    val ag: Aggregate[Int] = 5
    ag.repeat().take(4).map(_.top.concrete) shouldBe List(5.concrete, 5.concrete, 5.concrete, 5.concrete)

  test("operation on value"):
    val nv: NValue[Int] = 4
    val ag: Aggregate[Int] = for n <- nv yield n + 1
    ag.repeat().take(4).map(_.top.concrete) shouldBe List(5.concrete, 5.concrete, 5.concrete, 5.concrete)

  test("operation on constant"):
    val nv: NValue[Int] = 4
    val ag1: Aggregate[Int] = nv
    val ag: Aggregate[Int] = for n <- ag1 yield for v <- n yield v + 1
    ag.repeat().take(4).map(_.top.concrete) shouldBe List(5.concrete, 5.concrete, 5.concrete, 5.concrete)

  test("self on dummy nvalue"):
    val nv: NValue[Int] = 5
    val ag: Aggregate[Int] = self(nv)
    ag.repeat().take(4).map(_.top.concrete) shouldBe List(5.concrete, 5.concrete, 5.concrete, 5.concrete)

  test("Trees of a constant rep"):
    val ag: Aggregate[Int] = rep(5)(identity)
    ag.repeat().take(4).map(_.top.concrete) shouldBe List(5.concrete, 5.concrete, 5.concrete, 5.concrete)

  test("Trees of a counter"):
    val ag = rep(5)(n => for i <- n yield i + 1)
    ag.repeat().take(3).map(_.top.concrete) shouldBe List(6.concrete, 7.concrete, 8.concrete)

  test("Trees of a counter, with restart"):
    withDomainChange({case 3 | 4 | 5 => Set()}):
      val ag = rep(5)(n => for i <- n yield i + 1)
      ag.repeat().take(8).map(_.top.concrete) shouldBe List(6.concrete, 7.concrete, 8.concrete, 6.concrete, 6.concrete, 6.concrete, 7.concrete, 8.concrete)

  test("Trees of a nested counter"):
    rep(0): n =>
      for
        c <- rep(1)(identity)
        vc <- c
        vn <- n
      yield vn + vc
    .repeat().take(3).map(_.top.concrete) shouldBe Seq(1.concrete, 2.concrete, 3.concrete)

  test("Trees of a call to a non-aggregate program"):
    val f: () => Aggregate[Int] = () => 1.nv
    call(f).repeat().take(3).map(_.top.concrete) shouldBe Seq(1.concrete, 1.concrete, 1.concrete)

  test("Trees of a call to an aggregate program"):
    val f:() => Aggregate[Int] = () => counter(0)
    call(f).repeat().take(3).map(_.top.concrete) shouldBe Seq(1.concrete, 2.concrete, 3.concrete)

  test("Trees of a rep used to compute a boolean"):
    val ag = for
      c <- counter(0)
    yield for
      vc <- c
    yield vc < 3
    ag.repeat().take(6).map(_.top.concrete) shouldBe Seq(true.concrete, true.concrete, false.concrete, false.concrete, false.concrete, false.concrete)

  test("Results of muxing with a rep"):
    import AggregateLib.mux
    val agb: Aggregate[Boolean] = for
      c <- counter(0)
    yield for
      vc <- c
    yield vc < 3
    val ag = mux(agb)(1)(2)
    ag.repeat().take(4).toList.map(_.top.concrete) shouldBe List(1.concrete, 1.concrete, 2.concrete, 2.concrete)

  test("Results of muxing a counter with a rep"):
    import AggregateLib.mux
    val agb = for
      c <- counter(0)
    yield for
      vc <- c
    yield vc < 3 || vc > 4
    val ag = mux(agb)(counter(0))(100)
    // could also test a match
    ag.repeat().take(6).toList.map(_.top.concrete) shouldBe List(1.concrete, 2.concrete, 100.concrete, 100.concrete, 5.concrete, 6.concrete)

  test("Results of branching a counter with a rep"):
    import AggregateLib.branch
    val agb = for
      c <- counter(0)
    yield for
      vc <- c
    yield vc < 3 || vc > 4
    val ag = branch(agb)(counter(0))(100)
    ag.repeat().take(6).toList.map(_.top.concrete) shouldBe List(1.concrete, 2.concrete, 100.concrete, 100.concrete, 1.concrete, 2.concrete)

  test("Interaction with a neighbour, as a system"):
    val ag = retsend(0)(for i <- _ yield i + 1)
    val (d1, d2, d3) = (newDevice(), newDevice(), newDevice())
    val ds = DistributedSystem[Int](ag, Map(d1 -> Set(d1, d2, d3), d2 -> Set(d1, d2, d3), d3 -> Set(d1, d2, d3)))
    ds.fire(d1).top.concrete shouldBe 1.concrete
    ds.fire(d2).top.concrete shouldBe NValueConcrete(1, Map(d1 -> 2, d2 -> 1))
    ds.fire(d2).top.concrete shouldBe NValueConcrete(1, Map(d1 -> 2, d2 -> 2))
    ds.fire(d2).top.concrete shouldBe NValueConcrete(1, Map(d1 -> 2, d2 -> 3))
    ds.fire(d1).top.concrete shouldBe NValueConcrete(1, Map(d1 -> 2, d2 -> 3))
    ds.fire(d2).top.concrete shouldBe NValueConcrete(1, Map(d1 -> 4, d2 -> 4))
    ds.fire(d2).top.concrete shouldBe NValueConcrete(1, Map(d1 -> 4, d2 -> 5))
    ds.fire(d2).top.concrete shouldBe NValueConcrete(1, Map(d1 -> 4, d2 -> 6))
    ds.fire(d1).top.concrete shouldBe NValueConcrete(1, Map(d1 -> 3, d2 -> 5))
    ds.fire(d3).top.concrete shouldBe NValueConcrete(1, Map(d1 -> 2, d2 -> 2, d3 -> 1))
    ds.fire(d3).top.concrete shouldBe NValueConcrete(1, Map(d1 -> 2, d2 -> 2, d3 -> 2))
    ds.fire(d1).top.concrete shouldBe NValueConcrete(1, Map(d1 -> 4, d2 -> 5, d3 -> 3))

  test("Interaction with a neighbour, branched"):
    import AggregateLib.branch
    var cond = false
    val agf = branch(compute(cond))(0):
      exchange(0):n =>
        val rs = for i <- n yield i + 1
        (rs, rs)
    val (d1, d2) = (newDevice(), newDevice())
    val ds = DistributedSystem[Int](agf, Map(d1 -> Set(d1, d2), d2 -> Set(d1, d2)))
    ds.fire(d1).top.concrete shouldBe 1.concrete
    ds.fire(d2).top.concrete shouldBe NValueConcrete(1, Map(d1 -> 2))
    ds.fire(d2).top.concrete shouldBe NValueConcrete(1, Map(d1 -> 2, d2 -> 2))
    cond = true // sensor change
    ds.fire(d2).top.concrete shouldBe 0.concrete
    cond = false // sensor change
    ds.fire(d1).top.concrete shouldBe NValueConcrete(1, Map(d1 -> 2))
    ds.fire(d2).top.concrete shouldBe NValueConcrete(1, Map(d1 -> 2))
    ds.fire(d2).top.concrete shouldBe NValueConcrete(1, Map(d1 -> 2, d2 -> 2))

  test("Folding to get maximum neighbour fires"):
    import AggregateLib.branch
    val ag = fold[Int](0)(_ max _):
      retsend(0)(for i <- _ yield i + 1)
    val (d1, d2, d3) = (newDevice(), newDevice(), newDevice())
    val ds = DistributedSystem[Int](ag, Map(d1 -> Set(d1, d2, d3), d2 -> Set(d1, d2, d3), d3 -> Set(d1, d2, d3)))
    ds.fire(d1).top.concrete shouldBe 0.concrete
    ds.fire(d2).top.concrete shouldBe 2.concrete
    ds.fire(d2).top.concrete shouldBe 2.concrete
    ds.fire(d1).top.concrete shouldBe 3.concrete
    ds.fire(d2).top.concrete shouldBe 4.concrete