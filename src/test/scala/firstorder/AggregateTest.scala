package firstorder

import firstorder.AggregateTestUtilities.{*, given}
import firstorder.Aggregates.{*, given}
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers.*

class AggregateTest extends org.scalatest.funsuite.AnyFunSuite:

  test("Trees of a constant rep"):
    val ag = rep(5)(identity)
    ag.repeat().take(3) shouldBe Seq(
      TRep(5, TEmpty()),
      TRep(5, TVal(5)),
      TRep(5, TVal(5)))

  test("Trees of a constant rep with restart"):
    withDomainChange({case 3 | 4 | 5 => Set()}):
      rep(5)(identity).repeat().take(6).zipWithIndex.map((x, y) => y -> x) shouldBe Seq(
        0 -> TRep(5, TEmpty()),
        1 -> TRep(5, TVal(5)),
        2 -> TRep(5, TVal(5)),
        3 -> TRep(5, TEmpty()),
        4 -> TRep(5, TEmpty()),
        5 -> TRep(5, TEmpty()))

  test("Trees of a counter, with restart"):
    withDomainChange({case 3 | 4 | 5 => Set()}):
      rep(5)(_ + 1).repeat().take(9).zipWithIndex.map((x, y) => y -> x) shouldBe Seq(
        0 -> TRep(5, TEmpty()),
        1 -> TRep(6, TVal(6)),
        2 -> TRep(7, TVal(7)),
        3 -> TRep(5, TEmpty()),
        4 -> TRep(5, TEmpty()),
        5 -> TRep(5, TEmpty()),
        6 -> TRep(6, TVal(6)),
        7 -> TRep(7, TVal(7)),
        8 -> TRep(8, TVal(8)))

  test("Trees of a nested counter"):
    rep(0): n =>
      for
        c <- rep(1)(identity)
      yield c + n
    .repeat().take(3) shouldBe Seq(
      TRep(0, TEmpty()),
      TRep(1, TNext(TRep(1, TEmpty()), TVal(1))),
      TRep(2, TNext(TRep(1, TVal(1)), TVal(2))))

  test("Trees of a call to a non-aggregate program"):
    val f: () => Aggregate[Int] = () => 1
    call(f).repeat().take(2) shouldBe Seq(
      TCall(TVal(f), TVal(1)),
      TCall(TVal(f), TVal(1)))

  test("Trees of a call to an aggregate program"):
    val f = () => rep(0)(_ + 1)
    call(f).repeat().take(2) shouldBe Seq(
      TCall(TVal(f), TRep(0, TEmpty())),
      TCall(TVal(f), TRep(1, TVal(1))))

  test("Trees of a rep restarting due to a synthetic function change"):
    val f1 = () => rep(0)(_ + 1)
    val f2 = () => rep(0)(_ + 1)
    val c1 = call(f1)
    val c2 = call(f2)
    c1.evalOne(TCall(TVal(f1), TRep(0, TEmpty()))) shouldBe TCall(TVal(f1), TRep(1, TVal(1)))
    c2.evalOne(TCall(TVal(f1), TRep(0, TEmpty()))) shouldBe TCall(TVal(f2), TRep(0, TEmpty()))

  test("Trees of a rep used to compute a boolean"):
    val ag = for c <- rep(0)(_ + 1) yield c < 3
    ag.repeat().take(6) shouldBe Seq(
      TNext(TRep(0, TEmpty()), TVal(true)),
      TNext(TRep(1, TVal(1)), TVal(true)),
      TNext(TRep(2, TVal(2)), TVal(true)),
      TNext(TRep(3, TVal(3)), TVal(false)),
      TNext(TRep(4, TVal(4)), TVal(false)),
      TNext(TRep(5, TVal(5)), TVal(false)))

  test("Results of muxing with a rep"):
    import firstorder.Lib.*
    val ag = mux(for c <- rep(0)(_ + 1) yield c < 2)(1)(2)
    // could also test a match
    ag.repeat().take(4).toList.map(_.top) shouldBe List(1, 1, 2, 2)

  test("Results of muxing a counter with a rep"):
    import firstorder.Lib.*
    val ag = mux(for c <- rep(0)(_ + 1) yield c < 2 || c > 3)(counter)(100)
    ag.repeat().take(8).toList.map(_.top) shouldBe List(0, 1, 100, 100, 4, 5, 6, 7)

  test("Trees of a simple branch"):
    import firstorder.Lib.*
    branch(true)("a")("b").repeat().take(2).toList match
      case List(TCall(f, TVal("a")), TCall(g, TVal("a"))) if f.top == g.top =>
    branch(false)("a")("b").repeat().take(2).toList match
      case List(TCall(f, TVal("b")), TCall(g, TVal("b"))) if f.top == g.top =>

  test("Trees a fixed branch with counter"):
    import firstorder.Lib.*
    val ag = branch(true)(counter)(100)
    ag.repeat().take(2).toList match
      case List(
        TCall(TNext(TVal(true), _), TRep(0, TEmpty())),
        TCall(TNext(TVal(true), _), TRep(1, TVal(1))),
      ) =>

  test("Results Branching with a rep, checking reps inside"):
    import firstorder.Lib.*
    val ag = branch(for c <- rep(0)(_ + 1) yield c < 2 || c > 3)(counter(0))(counter(100))
    ag.repeat().take(8).toList.map:
      case TCall(_, res) => res.top
    .shouldBe(List(0, 1, 100, 101, 0, 1, 2, 3))

/*
test("Nvalues and AC"):
  import NValues.{*, given}

  val a: Aggregate[NValue[Int]] =
    rep(0 |> (1 -> 100)): v =>
      for
        x <- v
        y <- 0 |> (1 -> 1)
      yield x + y

  a.repeat().take(3).toList.map(_.top) shouldBe
    List(0 |> (1 -> 100), 0 |> (1 -> 101), 0 |> (1 -> 102))

  val a2: Aggregate[NValue[Int]] = rep[NValue[Int]](0): x =>
    for
      v <- x
    yield v + 1

  val a3: Aggregate[NValue[Int]] = rep[NValue[Int]](0): x =>
    for
      v <- compute(x)
      v2 <- rep[NValue[Int]](1)(identity)
    yield v.map2(v2)(_ + _)

  */