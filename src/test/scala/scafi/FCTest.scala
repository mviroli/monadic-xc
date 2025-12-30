package scafi

import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers.*
import scafi.experiments.FCEngineModule.{*, given}
import scafi.facade.Executor.*
import scafi.facade.Executor.DistributedSystem.platformSensor
import scafi.utils.MapWithDefault

class FCTest extends org.scalatest.funsuite.AnyFunSuite:

  test("pre"):
    val ag: Field[Int] = 5
    ag.evalOne(using selfDevice)()

  test("value"):
    val ag: Field[Int] = 5
    ag.repeat().take(4).map(_.top.asValue) shouldBe List(5, 5, 5, 5)

  test("operation on constant"):
    val ag1: Field[Int] = 4
    val ag: Field[Int] = for n <- ag1 yield n + 1
    ag.repeat().take(4).map(_.top.asValue) shouldBe List(5, 5, 5, 5)

  test("sensor"):
    var sns = true
    val ag: Field[Boolean] = localSensor(sns)
    ag.repeat().take(4).map(_.top.asValue) shouldBe List(true, true, true, true)
    sns = false
    ag.repeat().take(4).map(_.top.asValue) shouldBe List(false, false, false, false)

  test("Constant rep"):
    import scafi.lib.AggregateLib.rep
    val ag: Field[Int] = rep(5)(identity)
    ag.repeat().take(4).map(_.top.asValue) shouldBe List(5, 5, 5, 5)

  test("Counter"):
    val ag = rep(5)(n => n + 1)
    ag.repeat().take(4).map(_.top.asValue) shouldBe List(6, 7, 8, 9)

  test("Counter, with restart"):
    withDomainChange({case 3 | 4 | 5 => Set()}):
      val ag = rep(5)(n => n + 1)
      ag.repeat().take(8).map(_.top.asValue) shouldBe List(6, 7, 8, 6, 6, 6, 7, 8)

  test("Nested counter"):
    val ag = rep(0)(n => for i <- rep(1)(identity) yield i + n)
    ag.repeat().take(4).map(_.top.asValue) shouldBe List(1, 2, 3, 4)

  test("Elaborating on a rep")
    val ag = for
      c <- rep(0)(_ + 1)
    yield c < 3
    ag.repeat().take(4).map(_.top.asValue) shouldBe List(true, true, false, false)

  test("Muxing by a rep"):
    def mux[A](fb: Field[Boolean])(th: Field[A])(el: Field[A]): Field[A] =
      for
        b <- fb
        t <- th
        e <- el
      yield if b then t else e
    val ag = mux(for c <- rep(0)(_ + 1) yield c < 3)(1)(2)
    ag.repeat().take(4).map(_.top.asValue) shouldBe List(1, 1, 2, 2)

  test("Muxing with restart"):
    def mux[A](fb: Field[Boolean])(th: Field[A])(el: Field[A]): Field[A] =
      for
        b <- fb
        t <- th
        e <- el
      yield if b then t else e
    val ag = mux(for c <- rep(0)(_ + 1) yield c < 3 || c > 4)(rep(0)(_+1))(100)
    ag.repeat().take(6).map(_.top.asValue) shouldBe List(1, 2, 100, 100, 5, 6)

  test("Branching with restart"):
    val ag = branch(for c <- rep(0)(_ + 1) yield c < 3 || c > 4)(rep(0)(_ + 1))(100)
    ag.repeat().take(6).map(_.top.asValue) shouldBe List(1, 2, 100, 100, 1, 2)
/*
  test("Count neighbours"):
    val ag = fold(1)(_+_)(1) // val ag = fold(1)(_+_)(nbr(1)) is the same
    val (d1, d2, d3) = (newDevice(), newDevice(), newDevice())
    val ds = DistributedSystem(ag, Map(d1 -> Set(d1, d2, d3), d2 -> Set(d2, d1), d3 -> Set(d1, d3)))
    ds.fire(d2).top.asValue shouldBe 1
    ds.fire(d2).top.asValue shouldBe 1
    ds.fire(d3).top.asValue shouldBe 1
    ds.fire(d1).top.asValue shouldBe 3
    ds.fire(d2).top.asValue shouldBe 2
    ds.fire(d3).top.asValue shouldBe 2

  test("distributed sensor, and summing"):
    val (d1, d2, d3) = (newDevice(), newDevice(), newDevice())
    val ds: DistributedSystem[Int] = DistributedSystem(topology = Map(d1 -> Set(d1, d2, d3), d2 -> Set(d2, d1), d3 -> Set(d1, d3)))
          .withSensor("s1", Map(d1 -> 1, d2 -> 2, d3 -> 3))
          .withAggregate:
            def summer(sns: Field[Int]): Field[Int] = fold[Int](0)(_+_)(nbr(sns))
            summer(localSensor(platformSensor("s1")))
    ds.fire(d1).top.asValue shouldBe 0
    ds.fire(d2).top.asValue shouldBe 1
    ds.fire(d3).top.asValue shouldBe 1
    ds.fire(d1).top.asValue shouldBe 5

  test("mid"):
    val (d1, d2, d3) = (newDevice(), newDevice(), newDevice())
    val ds: DistributedSystem[Set[Device]] = DistributedSystem(topology = Map(d1 -> Set(d1, d2, d3), d2 -> Set(d2, d1), d3 -> Set(d1, d3)))
      .withSensor("mid", Map(d1 -> d1, d2 -> d2, d3 -> d3))
      .withAggregate:
        def summer(sns: Field[Device]): Field[Set[Device]] = fold[Set[Device]](Set())(_ ++ _)(nbr(sns map (Set(_))))
        summer(localSensor(platformSensor("mid")))
    ds.fire(d1).top.asValue shouldBe Set()
    ds.fire(d2).top.asValue shouldBe Set(d1)
    ds.fire(d3).top.asValue shouldBe Set(d1)
    ds.fire(d1).top.asValue shouldBe Set(d2, d3)

  test("gradient"):
    val (d1, d2, d3, d4, d5) = (newDevice(), newDevice(), newDevice(), newDevice(), newDevice())
    def mux[A](fb: Field[Boolean])(th: Field[A])(el: Field[A]): Field[A] =
      for
        b <- fb
        t <- th
        e <- el
      yield if b then t else e
    def gradient(src: Field[Boolean]): Field[Double] =
      rep(Double.PositiveInfinity): distance =>
        mux(src)(0.0):
          fold(Double.PositiveInfinity)(_ min _)(nbr(distance + 1))
    val ds = DistributedSystem(
      topology = Map(
        d1 -> Set(d1, d2, d4),
        d2 -> Set(d2, d3),
        d3 -> Set(d3, d4, d2),
        d4 -> Set(d4, d5, d3, d1),
        d5 -> Set(d5, d4)))
    .withSensor("mid", Map(d1 -> d1, d2 -> d2, d3 -> d3, d4 -> d4, d5 -> d5))
    .withAggregate:
        gradient(localSensor(platformSensor("mid")).map(_ == d1))
*/