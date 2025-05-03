package scafi

import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers.*
import scafi.AggregateTestUtilities.{*, given}
import scafi.AggregatesNFWithFree.{*, given}

class AggregateTest extends org.scalatest.funsuite.AnyFunSuite:
  import Tree.*
  extension [A](a: A) def nv: NValue[A] = NValue(a, Map.empty)
  def counter(initial: Int) = rep(initial)(for i <- _ yield i + 1)

  test("Trees of a constant rep"):
    val ag: Aggregate[Int] = rep(5)(identity)
    ag.repeat().take(3) shouldBe Seq(
      TRep(5.nv, TEmpty()),
      TRep(5.nv, TVal(5.nv)),
      TRep(5.nv, TVal(5.nv)))

  test("Trees of a constant rep with restart"):
    withDomainChange({case 3 | 4 | 5 => Set()}):
      rep(5)(identity).repeat().take(6).zipWithIndex.map((x, y) => y -> x) shouldBe Seq(
        0 -> TRep(5.nv, TEmpty()),
        1 -> TRep(5.nv, TVal(5.nv)),
        2 -> TRep(5.nv, TVal(5.nv)),
        3 -> TRep(5.nv, TEmpty()),
        4 -> TRep(5.nv, TEmpty()),
        5 -> TRep(5.nv, TEmpty()))

  test("Trees of a counter, with restart"):
    withDomainChange({case 3 | 4 | 5 => Set()}):
      rep(5)(n => for i <- n yield i + 1).repeat().take(9).zipWithIndex.map((x, y) => y -> x) shouldBe Seq(
        0 -> TRep(5.nv, TEmpty()),
        1 -> TRep(6.nv, TVal(6.nv)),
        2 -> TRep(7.nv, TVal(7.nv)),
        3 -> TRep(5.nv, TEmpty()),
        4 -> TRep(5.nv, TEmpty()),
        5 -> TRep(5.nv, TEmpty()),
        6 -> TRep(6.nv, TVal(6.nv)),
        7 -> TRep(7.nv, TVal(7.nv)),
        8 -> TRep(8.nv, TVal(8.nv)))

  test("Trees of a nested counter"):
    rep(0): n =>
      for
        c <- rep(1)(identity)
        vc <- c
        vn <- n
      yield vn + vc
    .repeat().take(3) shouldBe Seq(
      TRep(0.nv, TEmpty()),
      TRep(1.nv, TNext(TRep(1.nv, TEmpty()), TVal(1.nv))),
      TRep(2.nv, TNext(TRep(1.nv, TVal(1.nv)), TVal(2.nv))))

  test("Trees of a call to a non-aggregate program"):
    val f: () => Aggregate[Int] = () => 1.nv
    call(f).repeat().take(2) shouldBe Seq(
      TCall(TVal(f), TVal(1.nv)), // is that f ok???
      TCall(TVal(f), TVal(1.nv)))

  test("Trees of a call to an aggregate program"):
    val f:() => Aggregate[Int] = () => counter(0)
    call(f).repeat().take(2) shouldBe Seq(
      TCall(TVal(f), TRep(0.nv, TEmpty())),
      TCall(TVal(f), TRep(1.nv, TVal(1.nv))))


  test("Trees of a rep restarting due to a synthetic function change"):
    val f1 = () => counter(0)
    val f2 = () => counter(0)
    val c1 = call(f1)
    val c2 = call(f2)
    c1.evalOne(local(TCall(TVal(f1), TRep(0.nv, TEmpty())))) shouldBe TCall(TVal(f1), TRep(1.nv, TVal(1.nv)))
    c2.evalOne(local(TCall(TVal(f1), TRep(0.nv, TEmpty())))) shouldBe TCall(TVal(f2), TRep(0.nv, TEmpty()))

  test("Trees of a rep used to compute a boolean"):
    val ag = for
      c <- counter(0)
    yield for
      vc <- c
    yield vc < 3
    ag.repeat().take(6) shouldBe Seq(
      TNext(TRep(0.nv, TEmpty()), TVal(true.nv)),
      TNext(TRep(1.nv, TVal(1.nv)), TVal(true.nv)),
      TNext(TRep(2.nv, TVal(2.nv)), TVal(true.nv)),
      TNext(TRep(3.nv, TVal(3.nv)), TVal(false.nv)),
      TNext(TRep(4.nv, TVal(4.nv)), TVal(false.nv)),
      TNext(TRep(5.nv, TVal(5.nv)), TVal(false.nv)))

  test("Results of muxing with a rep"):
    import NValueLib.mux
    val agb: Aggregate[Boolean] = for
      c <- counter(0)
    yield for
      vc <- c
    yield vc < 2
    val ag = mux(agb)(1)(2)
    // could also test a match
    ag.repeat().take(4).toList.map(_.top) shouldBe List(1.nv, 1.nv, 2.nv, 2.nv)

  test("Results of muxing a counter with a rep"):
    import NValueLib.mux
    val agb = for
      c <- counter(0)
    yield for
      vc <- c
    yield vc < 2 || vc > 3
    val ag = mux(agb)(counter(0))(100)
    // could also test a match
    ag.repeat().take(6).toList.map(_.top) shouldBe List(0.nv, 1.nv, 100.nv, 100.nv, 4.nv, 5.nv)

  test("Results of branching a counter with a rep"):
    import NValueLib.branch
    val agb = for
      c <- counter(0)
    yield for
      vc <- c
    yield vc < 2 || vc > 3
    val ag = branch(agb)(counter(0))(100)
    // could also test a match
    ag.repeat().take(6).toList.map(_.top) shouldBe List(0.nv, 1.nv, 100.nv, 100.nv, 0.nv, 1.nv)


  /*
  test("Trees of a simple branch"):
    import Lib.*
    branch(true)("a")("b").repeat().take(2).toList match
      case List(TCall(f, TVal("a")), TCall(g, TVal("a"))) if f.top == g.top =>
    branch(false)("a")("b").repeat().take(2).toList match
      case List(TCall(f, TVal("b")), TCall(g, TVal("b"))) if f.top == g.top =>

  test("Trees a fixed branch with counter"):
    import Lib.*
    val ag = branch(true)(counter)(100)
    ag.repeat().take(2).toList match
      case List(
        TCall(TNext(TVal(true), _), TRep(0, TEmpty())),
        TCall(TNext(TVal(true), _), TRep(1, TVal(1))),
      ) =>

  test("Results Branching with a rep, checking reps inside"):
    import Lib.*
    val ag = branch(for c <- rep(0)(_ + 1) yield c < 2 || c > 3)(counter(0))(counter(100))
    ag.repeat().take(8).toList.map:
      case TCall(_, res) => res.top
    .shouldBe(List(0, 1, 100, 101, 0, 1, 2, 3))


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