package monadicXc

import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers.*


class BasicAggregateTest extends org.scalatest.funsuite.AnyFunSuite:

  val sut = new MonadicFramework with Lib with Executor
  import sut.{*, given}

  test("pre"):
    val ag: Aggregate[Int] = 5 // pureConversion(5)
    evalOnce(ag) shouldBe 5

  test("value"):
    val ag: Aggregate[Int] = 5
    repeat(ag).take(4) shouldBe List(5, 5, 5, 5)

  test("operation on constant"):
    val ag1: Aggregate[Int] = 4
    val ag: Aggregate[Int] = for n <- ag1 yield n + 1
    repeat(ag).take(4) shouldBe List(5, 5, 5, 5)

  test("constant rep"):
    val ag: Aggregate[Int] = rep(5)(identity)
    repeat(ag).take(4) shouldBe List(5, 5, 5, 5)

  test("counter"):
    val ag: Aggregate[Int] = rep(0)(_ + 1)
    repeat(ag).take(4) shouldBe List(1, 2, 3, 4)

  test("nested counter"):
    val ag: Aggregate[Int] = rep(0)(i => rep(0)(_ + 1).map(_ + i))
    repeat(ag).take(4) shouldBe List(1, 3, 6, 10)

  test("combining counters"):
    val ag: Aggregate[Int] = for
      i <- rep(0)(_ + 1)
      j <- rep(0)(_ + 2)
    yield i + j
    repeat(ag).take(4) shouldBe List(3, 6, 9, 12)

  test("counter with restart"):
    withDomainReset(List(3,4)):
      val ag: Aggregate[Int] = rep(0)(_ + 1)
      repeat(ag).take(6) shouldBe List(1, 2, 3, 1, 1, 2)

  test("Sensor"):
    var condition = true
    val ag = sensor(() => condition)
    val results = repeat(ag).take(4)
    results(0) shouldBe true
    results(1) shouldBe true
    condition = false
    results(2) shouldBe false
    results(3) shouldBe false

  test("Branch"):
    var condition = true
    val ag = sensor(() => condition).flatMap(b => branch(b)(counter)(0))
    val results = repeat(ag).take(6)
    results(0) shouldBe 1
    results(1) shouldBe 2
    condition = false
    results(2) shouldBe 0
    results(3) shouldBe 0
    condition = true
    results(4) shouldBe 1
    results(5) shouldBe 2


  test("pre-Mux"):
    var condition = true
    val ag = for
      i <- rep(0)(_ + 2)
      j <- rep(0)(_ + 1)
    yield if j<=2 then j else i
    repeat(ag).take(4) shouldBe List(1, 2, 6, 8)

  test("Mux"):
    var condition = true
    val ag = mux(sensor(() => condition))(counter)(0)
    val results = repeat(ag).take(6)
    results(0) shouldBe 1
    results(1) shouldBe 2
    condition = false
    results(2) shouldBe 0
    results(3) shouldBe 0
    condition = true
    results(4) shouldBe 5
    results(5) shouldBe 6



/*
test("Elaborating on a rep")

import scafi.lib.AggregateLib.counter

val ag = counter(0).aMapN(_ < 3)
ag.repeat().take(4).map(_.top.asValue) shouldBe List(true, true, false, false)


test("Call to a non-aggregate program"):
  val f: () => Aggregate[Int] = () => 5
  val ag = call(f)
  ag.repeat().take(4).map(_.top.asValue) shouldBe List(5, 5, 5, 5)


test("Call to an aggregate program"):
  val ag = call(() => counter(0))
  ag.repeat().take(4).map(_.top.asValue) shouldBe List(1, 2, 3, 4)

test("Muxing by a rep"):
  val agb: Aggregate[Boolean] = counter(0).aMapN(_ < 3)
  val ag = mux(agb)(1)(2)
  ag.repeat().take(4).map(_.top.asValue) shouldBe List(1, 1, 2, 2)

test("Muxing with restart"):
  val agb = counter(0).aMapN(vc => vc < 3 || vc > 4)
  val ag = mux(agb)(counter(0))(100)
  ag.repeat().take(6).map(_.top.asValue) shouldBe List(1, 2, 100, 100, 5, 6)

test("Branching with restart"):
  val agb = counter(0).aMapN(vc => vc < 3 || vc > 4)
  val ag = branch(agb)(counter(0))(100)
  ag.repeat().take(6).map(_.top.asValue) shouldBe List(1, 2, 100, 100, 1, 2)

test("Ping-pong"):
  val ag = retsend(0)(_.aMapN(_ + 1))
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
  var cond = false
  val agf = branch(sensor(cond))(0)(retsend(0)(_.aMapN(_ + 1)))
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
  val ag = fold(0)(_ max _)(retsend(0)(_.aMapN(_+1)))

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

test("hopGradient"):
  val ds = Platform()
    .withNeighbourhood(d1 -> Set(d1, d2))
    .withNeighbourhood(d2 -> Set(d1, d2, d3))
    .withNeighbourhood(d3 -> Set(d2, d3, d4))
    .withNeighbourhood(d4 -> Set(d2, d3, d4))
    .withSensor("src", Map(d1 -> true, d2 -> false, d3 -> false, d4 -> false))
    .asDistributedSystem:
      def hopGradient(src: Aggregate[Boolean]): Aggregate[Int] =
        retsend(Int.MaxValue): v =>
          mux(src)(0):
            fold(Int.MaxValue)(_ min _):
              v.aMapN(n => if n == Int.MaxValue then n else n + 1)
      hopGradient(platformSensor("src"))

  Seq(
    d2 -> Int.MaxValue, d1 -> 0, d2 -> 1, d4 -> Int.MaxValue, d3 -> 2, d4 -> 3
  ).foreach: (device, result) =>
    ds.fire(device).top.asValue shouldBe result


test("mid"):
  val ds = platform3
    .withSensor("mid", Map(d1 -> 1, d2 -> 2, d3 -> 3))
    .asDistributedSystem[Int]:
      platformSensor("mid")
  ds.fires(d1, d2, d2, d1, d2, d3).map(_.top.asValue) shouldBe List(1, 2, 2, 1, 2, 3)

test("gatherMids"):
  val ds = platform3
    .withSensor("mid", Map(d1 -> 1, d2 -> 2, d3 -> 3))
    .asDistributedSystem[Set[Int]]:
      def mid: Aggregate[Int] = platformSensor("mid")
      fold(self(mid).aMapN(Set(_)))(_ ++ _)(nbr(mid).aMapN(Set(_)))
  ds.fires(d1, d2, d3, d1).map(_.top.asValue) shouldBe List(Set(1), Set(1,2), Set(1,2,3), Set(1,2,3))

test("gossipIds"):
  import scafi.facadeMonadXC.AggregateApplicativeSyntax.*
  val ds = platformAdHoc
    .withSensor("mid", Map(d1 -> 1, d2 -> 2, d3 -> 3, d4 -> 4))
    .asDistributedSystem[Set[Int]]:
      def mid: Aggregate[Int] = platformSensor("mid")
      def gossipEver[A](init: A)(op: (A, A) => A)(a: Aggregate[A]): Aggregate[A] =
        retsend[A](init)(v => fold( (self(v), self(a)).aMapN(op(_,_)))(op)(v))
      gossipEver[Set[Int]](Set())(_ ++ _)(mid.map(_.map(Set(_))))
  ds.fires(d1, d2, d3, d1, d2, d1, d4, d3, d2, d1).map(_.top.asValue) shouldBe
    List(Set(1), Set(1, 2), Set(1,2,3), Set(1,2), Set(1,2,3), Set(1,2,3), Set(1,2,3,4), Set(1,2,3,4), Set(1,2,3,4), Set(1,2,3,4))

test("gossipMinId"):
  import scafi.facadeMonadXC.AggregateApplicativeSyntax.*
  val ds = platformAdHoc
    .withSensor("mid", Map(d1 -> 1, d2 -> 2, d3 -> 3, d4 -> 4))
    .asDistributedSystem:
      def mid: Aggregate[Int] = platformSensor("mid")
      def gossipEver[A](init: A)(op: (A, A) => A)(a: Aggregate[A]): Aggregate[A] =
        retsend[A](init)(v => fold( (self(v), self(a)).aMapN(op(_,_)))(op)(v))
      gossipEver[Int](Int.MaxValue)(_ min _)(mid)

  Seq(
    d4 -> 4, d2 -> 2, d3 -> 2, d1 -> 1, d4 -> 2, d2 -> 1, d3 -> 1, d4 -> 1
  ).foreach: (device, result) =>
    ds.fire(device).top.asValue shouldBe result

*/