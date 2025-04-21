package others

import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers.*
import others.AggregateFramework.*
import scafi.NValues
import scafi.NValues.NValue
import scafi.Trees.Tree
import scafi.Trees.Tree.*


class RepTest extends org.scalatest.funsuite.AnyFunSuite:
  import others.AggregateFramework.Aggregate.given

  type DomainChange = PartialFunction[Int, Set[Device]]
  given DomainChange = Map.empty
  type TreeChange[A] = PartialFunction[Int, Tree[A]]
  given tc[A]:TreeChange[A] = Map.empty
  type Domain = Set[Device]

  def withDomainChange(domainChange: DomainChange)(body: DomainChange ?=> Assertion): Assertion = body(using domainChange)
  def withTreeChange[A](treeChange: TreeChange[A])(body: TreeChange[A] ?=> Assertion): Assertion = body(using treeChange)

  extension [A](a: Aggregate[A])
    def repeat(initial: Tree[A] = Empty[A](), dom: Domain = Set(selfDevice))
              (using domainChange: DomainChange)(using treeChange: TreeChange[A]): LazyList[Tree[A]] =
      LazyList.iterate((-1, initial)):
        (step, tree) => (step + 1, a.eval(tree)(using domainChange.applyOrElse(step + 1, _ => dom)))
      .map(_._2).drop(1)

    def evalOne(initial: Tree[A] = Empty(), dom: Domain = Set(selfDevice)): Tree[A] =
      repeat(initial, dom)(0)

  test("Constant rep"):
    val ag = rep(5)(identity)
    ag.repeat().take(3) shouldBe Seq(
      Rep(5, Empty()),
      Rep(5, Val(5)),
      Rep(5, Val(5)))

  test("Constant rep, restarting with temporary domain restriction advanced"):
    withDomainChange({case 3 | 4 | 5 => Set()}):
      rep(5)(identity).repeat().take(6).zipWithIndex.map((x, y) => y -> x) shouldBe Seq(
        0 -> Rep(5, Empty()),
        1 -> Rep(5, Val(5)),
        2 -> Rep(5, Val(5)),
        3 -> Rep(5, Empty()),
        4 -> Rep(5, Empty()),
        5 -> Rep(5, Empty()))

  test("Counter, also restarting"):
    withDomainChange({case 3 | 4 | 5 => Set()}):
      rep(5)(_ + 1).repeat().take(9).zipWithIndex.map((x, y) => y -> x) shouldBe Seq(
        0 -> Rep(5, Empty()),
        1 -> Rep(6, Val(6)),
        2 -> Rep(7, Val(7)),
        3 -> Rep(5, Empty()),
        4 -> Rep(5, Empty()),
        5 -> Rep(5, Empty()),
        6 -> Rep(6, Val(6)),
        7 -> Rep(7, Val(7)),
        8 -> Rep(8, Val(8)))

  test("Counter with nesting"):
    rep(0): n =>
      for
        c <- rep(1)(identity)
      yield c + n
    .repeat().take(3) shouldBe Seq(
      Rep(0, Empty()),
      Rep(1, Next(Rep(1, Empty()), Val(1))),
      Rep(2, Next(Rep(1, Val(1)), Val(2))))

  test("Counter used on a boolean"):
    rep(0)(_ + 1).map(_ < 2)
    .repeat().take(4) shouldBe Seq(
      Next(Rep(0, Empty()), Val(true)),
      Next(Rep(1, Val(1)), Val(true)),
      Next(Rep(2, Val(2)), Val(false)),
      Next(Rep(3, Val(3)), Val(false)))

  test("Call to a non-aggregate program"):
    val f = () => compute(1)
    call(f).repeat().take(2) shouldBe Seq(
      Call(Val(f), Val(1)),
      Call(Val(f), Val(1)))


  test("Call to an aggregate program"):
    val f = () => counter(0)
    call(f).repeat().take(2) shouldBe Seq(
      Call(Val(f), Rep(0, Empty())),
      Call(Val(f), Rep(1, Val(1))))

  test("Rep restarts in case of a synthetic function change"):
    val f1 = () => counter(0)
    val f2 = () => counter(0)
    val c1 = call(f1)
    val c2 = call(f2)
    c1.evalOne(Call(Val(f1), Rep(0, Empty()))) shouldBe Call(Val(f1), Rep(1, Val(1)))
    c2.evalOne(Call(Val(f1), Rep(0, Empty()))) shouldBe Call(Val(f2), Rep(0, Empty()))


  test("Rep used to compute a boolean"):
    val ag = for c <- rep(0)(_ + 1) yield c < 3
    ag.repeat().take(6) shouldBe Seq(
      Next(Rep(0, Empty()), Val(true)),
      Next(Rep(1, Val(1)), Val(true)),
      Next(Rep(2, Val(2)), Val(true)),
      Next(Rep(3, Val(3)), Val(false)),
      Next(Rep(4, Val(4)), Val(false)),
      Next(Rep(5, Val(5)), Val(false)))

  test("Muxing with a rep"):
    import AdditionalConstructs.*
    val ag = mux(for c <- rep(0)(_ + 1) yield c < 2)(1)(2)
    ag.repeat().take(4).toList shouldBe List(
      Next(Next(Rep(0,Empty()),Val(true)),Next(Val(1),Next(Val(2),Val(1)))),
      Next(Next(Rep(1,Val(1)),Val(true)),Next(Val(1),Next(Val(2),Val(1)))),
      Next(Next(Rep(2, Val(2)), Val(false)), Next(Val(1), Next(Val(2), Val(2)))),
      Next(Next(Rep(3, Val(3)), Val(false)), Next(Val(1), Next(Val(2), Val(2))))
    )
    // could also test a match
    ag.repeat().take(4).toList.map(_.top) shouldBe List(1, 1, 2, 2)

  test("Muxing with a rep, checking reps inside"):
    import AdditionalConstructs.*
    val ag = mux(for c <- rep(0)(_ + 1) yield c < 2 || c > 3)(counter(0))(100)
    ag.repeat().take(8).toList.map(_.top) shouldBe List(0, 1, 100, 100, 4, 5, 6, 7)

  test("Simple branch"):
    import AdditionalConstructs.*
    branch(true)("a")("b").repeat().take(2).toList match
      case List(Call(f, Val("a")), Call(g, Val("a"))) if f.top == g.top =>
    branch(false)("a")("b").repeat().take(2).toList match
      case List(Call(f, Val("b")), Call(g, Val("b"))) if f.top == g.top =>

  test("Fixed branch with counter"):
    import AdditionalConstructs.*
    val ag = branch(true)(counter(0))(100)
    ag.repeat().take(2).toList match
      case List(
        Call(Next(Val(true), _), Rep(0, Empty())),
        Call(Next(Val(true), _), Rep(1, Val(1))),
      ) =>

  test("Branching with a rep, checking reps inside"):
    import AdditionalConstructs.*
    val ag = branch(for c <- rep(0)(_ + 1) yield c < 2 || c > 3)(counter(0))(counter(100))
    ag.repeat().take(8).toList.map:
      case Call(_, res) => res.top
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