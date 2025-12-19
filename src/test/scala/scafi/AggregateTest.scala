package scafi

import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers.*
import scafi.facade.Executor.*
import scafi.facade.AggregateEngineModule.{*, given}
import scafi.facade.Executor.DistributedSystem.bind
import scafi.utils.MapWithDefault

class AggregateTest extends org.scalatest.funsuite.AnyFunSuite:

  test("pre"):
    val ag: Aggregate[Int] = 5
    ag.evalOne(using selfDevice)()


  test("value"):
    val ag: Aggregate[Int] = 5
    ag.repeat().take(4).map(_.top.asValue) shouldBe List(5, 5, 5, 5)

  test("nvalue"):
    val nv: NValue[Int] = 5
    val ag: Aggregate[Int] = nv
    ag.repeat().take(4).map(_.top.asValue) shouldBe List(5, 5, 5, 5)


  test("operation on value"):
    val nv: NValue[Int] = 4
    val ag: Aggregate[Int] = for n <- nv yield n + 1
    ag.repeat().take(4).map(_.top.asValue) shouldBe List(5, 5, 5, 5)

  test("operation on constant"):
    val nv: NValue[Int] = 4
    val ag1: Aggregate[Int] = nv
    val ag: Aggregate[Int] = for n <- ag1 yield for v <- n yield v + 1
    ag.repeat().take(4).map(_.top.asValue) shouldBe List(5, 5, 5, 5)

  test("self on nvalue"):
    val nv: NValue[Int] = NValue(MapWithDefault(5, Map(newDevice() -> 4)))
    val ag: Aggregate[Int] = nself(nv)
    ag.repeat().take(4).map(_.top.asValue) shouldBe List(5, 5, 5, 5)

  test("sensor"):
    var sns = true
    val ag: Aggregate[Boolean] = sensor(sns)
    ag.repeat().take(4).map(_.top.asValue) shouldBe List(true, true, true, true)
    sns = false
    ag.repeat().take(4).map(_.top.asValue) shouldBe List(false, false, false, false)

  test("Constant rep"):
    import scafi.lib.AggregateLib.rep
    val ag: Aggregate[Int] = rep(5)(identity)
    ag.repeat().take(4).map(_.top.asValue) shouldBe List(5, 5, 5, 5)


  test("Counter"):
    import scafi.lib.AggregateLib.rep
    val ag = rep(5)(n => for i <- n yield i + 1)
    ag.repeat().take(4).map(_.top.asValue) shouldBe List(6, 7, 8, 9)

  test("Counter, with restart"):
    import scafi.lib.AggregateLib.rep
    withDomainChange({ case 3 | 4 | 5 => Set() }):
      val ag = rep(5)(n => for i <- n yield i + 1)
      ag.repeat().take(8).map(_.top.asValue) shouldBe List(6, 7, 8, 6, 6, 6, 7, 8)

  test("Nested counter"):
    import scafi.lib.AggregateLib.rep
    // rep(0)(n => rep(1)(identity) + n)
    val ag = rep(0): n =>
      for
        c <- rep(1)(identity)
        vc <- c
        vn <- n
      yield vn + vc
    ag.repeat().take(4).map(_.top.asValue) shouldBe List(1, 2, 3, 4)

  test("Elaborating on a rep")

  import scafi.lib.AggregateLib.counter

  val ag = for
    c <- counter(0)
  yield for
    vc <- c
  yield vc < 3
  ag.repeat().take(4).map(_.top.asValue) shouldBe List(true, true, false, false)

  test("Call to a non-aggregate program"):
    val f: () => Aggregate[Int] = () => 5
    val ag = call(f)
    ag.repeat().take(4).map(_.top.asValue) shouldBe List(5, 5, 5, 5)

  test("Call to an aggregate program"):
    import scafi.lib.AggregateLib.counter
    val ag = call(() => counter(0))
    ag.repeat().take(4).map(_.top.asValue) shouldBe List(1, 2, 3, 4)

  test("Muxing by a rep"):
    import scafi.lib.AggregateLib.mux
    val agb: Aggregate[Boolean] = for
      c <- counter(0)
    yield for
      vc <- c
    yield vc < 3
    val ag = mux(agb)(1)(2)
    ag.repeat().take(4).map(_.top.asValue) shouldBe List(1, 1, 2, 2)

  test("Muxing with restart"):
    import scafi.lib.AggregateLib.mux
    val agb = for
      c <- counter(0)
    yield for
      vc <- c
    yield vc < 3 || vc > 4
    val ag = mux(agb)(counter(0))(100)
    ag.repeat().take(6).map(_.top.asValue) shouldBe List(1, 2, 100, 100, 5, 6)

  test("Branching with restart"):
    import scafi.lib.AggregateLib.branch
    val agb = for
      c <- counter(0)
    yield for
      vc <- c
    yield vc < 3 || vc > 4
    val ag = branch(agb)(counter(0))(100)
    ag.repeat().take(6).map(_.top.asValue) shouldBe List(1, 2, 100, 100, 1, 2)

  test("Ping-pong"):
    import scafi.lib.AggregateLib.retsend
    val ag = retsend(0)(for i <- _ yield i + 1)
    val (d1, d2, d3) = (newDevice(), newDevice(), newDevice())
    val ds = Platform()
      .withNeighbourhood(d1 -> Set(d1, d2, d3))
      .withNeighbourhood(d2 -> Set(d1, d2, d3))
      .withNeighbourhood(d3 -> Set(d1, d2, d3))
      .asDistributedSystem(ag)
    ds.fire(d1).top.asValue shouldBe 1
    ds.fire(d2).top shouldBe MapWithDefault(1, Map(d1 -> 2, d2 -> 1))
    ds.fire(d2).top shouldBe MapWithDefault(1, Map(d1 -> 2, d2 -> 2))
    ds.fire(d2).top shouldBe MapWithDefault(1, Map(d1 -> 2, d2 -> 3))
    ds.fire(d1).top shouldBe MapWithDefault(1, Map(d1 -> 2, d2 -> 3))
    ds.fire(d2).top shouldBe MapWithDefault(1, Map(d1 -> 4, d2 -> 4))
    ds.fire(d2).top shouldBe MapWithDefault(1, Map(d1 -> 4, d2 -> 5))
    ds.fire(d2).top shouldBe MapWithDefault(1, Map(d1 -> 4, d2 -> 6))
    ds.fire(d1).top shouldBe MapWithDefault(1, Map(d1 -> 3, d2 -> 5))
    ds.fire(d3).top shouldBe MapWithDefault(1, Map(d1 -> 2, d2 -> 2, d3 -> 1))
    ds.fire(d3).top shouldBe MapWithDefault(1, Map(d1 -> 2, d2 -> 2, d3 -> 2))
    ds.fire(d1).top shouldBe MapWithDefault(1, Map(d1 -> 4, d2 -> 5, d3 -> 3))

  test("Branching ping-pong with a sensor"):
    import scafi.lib.AggregateLib.{branch, retsend}
    var cond = false
    val agf = branch(sensor(cond))(0)(retsend(0)(for i <- _ yield i + 1))
    val (d1, d2) = (newDevice(), newDevice())
    val ds = Platform()
      .withNeighbourhood(d1 -> Set(d1, d2))
      .withNeighbourhood(d2 -> Set(d1, d2))
      .asDistributedSystem(agf)
    ds.fire(d1).top.asValue shouldBe 1
    ds.fire(d2).top shouldBe MapWithDefault(1, Map(d1 -> 2))
    ds.fire(d2).top shouldBe MapWithDefault(1, Map(d1 -> 2, d2 -> 2))
    cond = true // sensor change
    ds.fire(d2).top.asValue shouldBe 0
    cond = false // sensor change
    ds.fire(d1).top shouldBe MapWithDefault(1, Map(d1 -> 2))
    ds.fire(d2).top shouldBe MapWithDefault(1, Map(d1 -> 2))
    ds.fire(d2).top shouldBe MapWithDefault(1, Map(d1 -> 2, d2 -> 2))

  test("Folding a ping-pong"):
    import scafi.lib.AggregateLib.retsend
    val ag = for
      n <- retsend(0)(for i <- _ yield i + 1)
    yield for
      i <- nfold(0)(_ max _)(n)
    yield i
    val (d1, d2, d3) = (newDevice(), newDevice(), newDevice())
    val ds = Platform()
      .withNeighbourhood(d1 -> Set(d1, d2, d3))
      .withNeighbourhood(d2 -> Set(d1, d2, d3))
      .withNeighbourhood(d3 -> Set(d1, d2, d3))
      .asDistributedSystem(ag)
    ds.fires(d1, d2, d2, d1, d2).map(_.top.asValue) shouldBe List(0, 2, 2, 3, 4)

  val List(d1, d2, d3, d4) = List.fill(4)(newDevice())
  val platform3 = Platform()
    .withNeighbourhood(d1 -> Set(d1, d2, d3))
    .withNeighbourhood(d2 -> Set(d1, d2, d3))
    .withNeighbourhood(d3 -> Set(d1, d2, d3))
  val platformAdHoc = Platform()
    .withNeighbourhood(d1 -> Set(d1, d2))
    .withNeighbourhood(d2 -> Set(d1, d2, d3))
    .withNeighbourhood(d3 -> Set(d2, d3, d4))
    .withNeighbourhood(d4 -> Set(d2, d3, d4))


  test("mid"):
    val ds = platform3
      .withSensor("mid", Map(d1 -> 1, d2 -> 2, d3 -> 3))
      .asDistributedSystem[Int]:
        sensor(bind("mid"))
    ds.fires(d1, d2, d2, d1, d2, d3).map(_.top.asValue) shouldBe List(1, 2, 2, 1, 2, 3)

  test("gatherMids"):
    import lib.AggregateLib.*
    val ds = platform3
      .withSensor("mid", Map(d1 -> 1, d2 -> 2, d3 -> 3))
      .asDistributedSystem[Set[Int]]:
        def mid: Aggregate[Int] = sensor(bind("mid"))
        for
          m <- mid
          v <- nbr(mid)
        yield nfold(Set(m.selfValue))(_ ++ _)(v.map(Set(_)))
    ds.fires(d1, d2, d3, d1).map(_.top.asValue) shouldBe List(Set(1), Set(1,2), Set(1,2,3), Set(1,2,3))

  test("gossipIds"):
    import lib.AggregateLib.*
    val ds = platformAdHoc
      .withSensor("mid", Map(d1 -> 1, d2 -> 2, d3 -> 3, d4 -> 4))
      .asDistributedSystem[Set[Int]]:
        def mid: Aggregate[Int] = sensor(bind("mid"))
        def gossipEver[A](init: A)(op: (A, A) => A)(a: Aggregate[A]): Aggregate[A] =
          for
            nva <- a
            g <- retsend[A](init)(v => nfold(op(v.selfValue, nva.selfValue))(op)(v))
          yield g
        gossipEver[Set[Int]](Set())(_ ++ _)(mid.map(_.map(Set(_))))
    ds.fires(d1, d2, d3, d1, d2, d1, d4, d3, d2, d1).map(_.top.asValue) shouldBe
      List(Set(1), Set(1, 2), Set(1,2,3), Set(1,2), Set(1,2,3), Set(1,2,3), Set(1,2,3,4), Set(1,2,3,4), Set(1,2,3,4), Set(1,2,3,4))

  test("gossipMinId"):
    import lib.AggregateLib.*
    val ds = platformAdHoc
      .withSensor("mid", Map(d1 -> 1, d2 -> 2, d3 -> 3, d4 -> 4))
      .asDistributedSystem:
        def mid: Aggregate[Int] = sensor(bind("mid"))
        def gossipEver[A](init: A)(op: (A, A) => A)(a: Aggregate[A]): Aggregate[A] =
          for
            nva <- a
            g <- retsend[A](init)(v => nfold(op(v.selfValue, nva.selfValue))(op)(v))
          yield g
        gossipEver[Int](Int.MaxValue)(_ min _)(mid)

    Seq(
      d4 -> 4, d2 -> 2, d3 -> 2, d1 -> 1, d4 -> 2, d2 -> 1, d3 -> 1, d4 -> 1
    ).foreach: (device, result) =>
      ds.fire(device).top.asValue shouldBe result

  test("gradient"):
    import lib.AggregateLib.*
    val ds = Platform()
      .withNeighbourhood(d1 -> Set(d1, d2))
      .withNeighbourhood(d2 -> Set(d1, d2, d3))
      .withNeighbourhood(d3 -> Set(d2, d3, d4))
      .withNeighbourhood(d4 -> Set(d2, d3, d4))
      .withSensor("src", Map(d1 -> true, d2 -> false, d3 -> false, d4 -> false))
      .asDistributedSystem:
        gradientHop(sensor(bind("src")))

    Seq(
      d2 -> Int.MaxValue, d1 -> 0, d2 -> 1, d4 -> Int.MaxValue, d3 -> 2, d4 -> 3
    ).foreach: (device, result) =>
      ds.fire(device).top.asValue shouldBe result