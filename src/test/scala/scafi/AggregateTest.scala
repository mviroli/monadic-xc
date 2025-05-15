package scafi

import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers.*
import scafi.AggregateTestUtilities.{*, given}
import scafi.Aggregates.{*, given}

class AggregateTest extends org.scalatest.funsuite.AnyFunSuite:
  import Tree.*
  def counter(initial: Int) = rep(initial)(for i <- _ yield i + 1)
  given Device = selfDevice

  test("Trees of a constant rep"):
    val ag: Aggregate[Int] = rep(5)(identity)
    ag.repeat().take(4).map(_.top) shouldBe List(5.nv, 5.nv, 5.nv, 5.nv)

  test("Trees of a counter"):
    val ag = rep(5)(n => for i <- n yield i + 1)
    ag.repeat().take(3).map(_.top) shouldBe List(6.nv, 7.nv, 8.nv)

  test("Trees of a counter, with restart"):
    withDomainChange({case 3 | 4 | 5 => Set()}):
      val ag = rep(5)(n => for i <- n yield i + 1)
      ag.repeat().take(8).map(_.top) shouldBe List(6.nv, 7.nv, 8.nv, 6.nv, 6.nv, 6.nv, 7.nv, 8.nv)

  test("Trees of a nested counter"):
    rep(0): n =>
      for
        c <- rep(1)(identity)
        vc <- c
        vn <- n
      yield vn + vc
    .repeat().take(3).map(_.top) shouldBe Seq(1.nv, 2.nv, 3.nv)

  test("Trees of a call to a non-aggregate program"):
    val f: () => Aggregate[Int] = () => 1.nv
    call(f).repeat().take(3).map(_.top) shouldBe Seq(1.nv, 1.nv, 1.nv)

  test("Trees of a call to an aggregate program"):
    val f:() => Aggregate[Int] = () => counter(0)
    call(f).repeat().take(3).map(_.top) shouldBe Seq(1.nv, 2.nv, 3.nv)

  test("Trees of a rep used to compute a boolean"):
    val ag = for
      c <- counter(0)
    yield for
      vc <- c
    yield vc < 3
    ag.repeat().take(6).map(_.top) shouldBe Seq(true.nv, true.nv, false.nv, false.nv, false.nv, false.nv)

  test("Results of muxing with a rep"):
    import AggregateLib.mux
    val agb: Aggregate[Boolean] = for
      c <- counter(0)
    yield for
      vc <- c
    yield vc < 3
    val ag = mux(agb)(1)(2)
    ag.repeat().take(4).toList.map(_.top) shouldBe List(1.nv, 1.nv, 2.nv, 2.nv)

  test("Results of muxing a counter with a rep"):
    import AggregateLib.mux
    val agb = for
      c <- counter(0)
    yield for
      vc <- c
    yield vc < 3 || vc > 4
    val ag = mux(agb)(counter(0))(100)
    // could also test a match
    ag.repeat().take(6).toList.map(_.top) shouldBe List(1.nv, 2.nv, 100.nv, 100.nv, 5.nv, 6.nv)

  test("Results of branching a counter with a rep"):
    import AggregateLib.branch
    val agb = for
      c <- counter(0)
    yield for
      vc <- c
    yield vc < 3 || vc > 4
    val ag = branch(agb)(counter(0))(100)
    ag.repeat().take(6).toList.map(_.top) shouldBe List(1.nv, 2.nv, 100.nv, 100.nv, 1.nv, 2.nv)

  test("Interaction with a neighbour"):
    val ag = exchange(0)(n => {val rs = for i <- n yield i + 1; (rs, rs)})
    val d1 = newDevice()
    val r1: Tree[Int] = ag.evalOne(using d1)(Map(d1 -> TEmpty()), Set(d1))
    r1.top shouldBe 1.nv
    val d2 = newDevice()
    val r2: Tree[Int] = ag.evalOne(using d2)(Map(d2 -> TEmpty(), d1 -> r1), Set(d1, d2))
    r2.top shouldBe NValue(1, Map(d1 -> 2))
    val r2b = ag.evalOne(using d2)(Map(d2 -> r2, d1 -> r1), Set(d1, d2))
    r2b.top shouldBe NValue(1, Map(d1 -> 2, d2 -> 2))
    ag.evalOne(using d2)(Map(d2 -> r2b, d1 -> r1), Set(d1, d2)).top shouldBe NValue(1, Map(d1 -> 2, d2 -> 3))

  test("Interaction with a neighbour, branched"):
    import AggregateLib.branch
    var cond = false
    def getCond = cond
    val agf = branch(compute(cond))(0):
      exchange(0):n =>
        val rs = for i <- n yield i + 1
        (rs, rs)
    val d1 = newDevice()
    val d2 = newDevice()
    val r1: Tree[Int] = agf.evalOne(using d1)(Map(d1 -> TEmpty()), Set(d1))
    r1.top shouldBe 1.nv
    val r2: Tree[Int] = agf.evalOne(using d2)(Map(d2 -> TEmpty(), d1 -> r1), Set(d1, d2))
    r2.top shouldBe NValue(1, Map(d1 -> 2))
    val r2b = agf.evalOne(using d2)(Map(d2 -> r2, d1 -> r1), Set(d1, d2))
    r2b.top shouldBe NValue(1, Map(d1 -> 2, d2 -> 2))
    cond = true
    val r2c = agf.evalOne(using d2)(Map(d2 -> r2b, d1 -> r1), Set(d1, d2))
    r2c.top shouldBe 0.nv
    cond = false
    val r1b = agf.evalOne(using d1)(Map(d2 -> r2c, d1 -> r1), Set(d1, d2))
    r1b.top shouldBe NValue(1, Map(d1 -> 2))
    val r2d = agf.evalOne(using d2)(Map(d2 -> r2c, d1 -> r1b), Set(d1, d2))
    r2d.top shouldBe NValue(1, Map(d1 -> 2))
