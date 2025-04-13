package scafi

import org.scalatest.matchers.should.Matchers.*
import AggregateFramework.*
import org.scalatest.Assertion


class RepTest extends org.scalatest.funsuite.AnyFunSuite:
  import AggregateFramework.Aggregate.given

  type DomainChange = PartialFunction[Int, Set[Device]]
  given DomainChange = Map.empty
  type TreeChange[A] = PartialFunction[Int, Tree[A]]
  given tc[A]:TreeChange[A] = Map.empty
  type Domain = Set[Device]

  def withDomainChange(domainChange: DomainChange)(body: DomainChange ?=> Assertion): Assertion = body(using domainChange)
  def withTreeChange[A](treeChange: TreeChange[A])(body: TreeChange[A] ?=> Assertion): Assertion = body(using treeChange)

  extension [A](a: Aggregate[A])
    def repeat(initial: Tree[A] = Empty[A](), dom: Domain = Set(selfDevice))
              (using domainChange: DomainChange)(using treeChange: TreeChange[A]): LazyList[(Int, Tree[A])] =
      LazyList.iterate((-1, initial)):
        (step, tree) => (step + 1, a.eval(tree)(using domainChange.applyOrElse(step + 1, _ => dom)))
      .drop(1)

    def evalOne(initial: Tree[A] = Empty(), dom: Domain = Set(selfDevice)): Tree[A] =
      repeat(initial, dom)(0)._2

  test("Constant rep"):
    val ag = rep(5)(identity)
    ag.repeat().take(3) shouldBe Seq(
      0 -> Rep(5, Empty()),
      1 -> Rep(5, Val(5)),
      2 -> Rep(5, Val(5)))

  test("Constant rep, restarting with temporary domain restriction advanced"):
    withDomainChange({case 3 | 4 | 5 => Set()}):
      rep(5)(identity).repeat().take(6) shouldBe Seq(
        0 -> Rep(5, Empty()),
        1 -> Rep(5, Val(5)),
        2 -> Rep(5, Val(5)),
        3 -> Rep(5, Empty()),
        4 -> Rep(5, Empty()),
        5 -> Rep(5, Empty()))

  test("Counter, also restarting"):
    withDomainChange({case 3 | 4 | 5 => Set()}):
      rep(5)(_ + 1).repeat().take(9) shouldBe Seq(
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
      0 -> Rep(0, Empty()),
      1 -> Rep(1, Next(1, Rep(1, Empty()), Val(1))),
      2 -> Rep(2, Next(2, Rep(1, Val(1)), Val(2))))

  test("Counter used on a boolean"):
    rep(0)(_ + 1).map(_ < 2)
    .repeat().take(4) shouldBe Seq(
      0 -> Next(true, Rep(0, Empty()), Val(true)),
      1 -> Next(true, Rep(1, Val(1)), Val(true)),
      2 -> Next(false, Rep(2, Val(2)), Val(false)),
      3 -> Next(false, Rep(3, Val(3)), Val(false)))

  test("Call to a non-aggregate program"):
    val f = () => compute(1)
    aggregateCall(f).repeat().take(2) shouldBe Seq(
      0 -> Call(f, Val(f), Val(1)),
      1 -> Call(f, Val(f), Val(1)))


  test("Call to an aggregate program"):
    val f = () => counter(0)
    aggregateCall(f).repeat().take(2) shouldBe Seq(
      0 -> Call(f, Val(f), Rep(0, Empty())),
      1 -> Call(f, Val(f), Rep(1, Val(1))))

  test("Rep restarts in case of a synthetic function change"):
    val f1 = () => counter(0)
    val f2 = () => counter(0)
    val c1 = aggregateCall(f1)
    val c2 = aggregateCall(f2)
    c1.evalOne(Call(f1, Val(f1), Rep(0, Empty()))) shouldBe Call(f1, Val(f1), Rep(1, Val(1)))
    c2.evalOne(Call(f1, Val(f2), Rep(0, Empty()))) shouldBe Call(f2, Val(f2), Rep(0, Empty()))


  test("Rep used to compute a boolean"):
    val ag = for c <- rep(0)(_ + 1) yield c < 3
    ag.repeat().take(6) shouldBe Seq(
      0 -> Next(true, Rep(0, Empty()), Val(true)),
      1 -> Next(true, Rep(1, Val(1)), Val(true)),
      2 -> Next(true, Rep(2, Val(2)), Val(true)),
      3 -> Next(false, Rep(3, Val(3)), Val(false)),
      4 -> Next(false, Rep(4, Val(4)), Val(false)),
      5 -> Next(false, Rep(5, Val(5)), Val(false)))

  test("Muxing with a rep"):
    import AdditionalConstructs.*
    val ag = mux(for c <- rep(0)(_ + 1) yield c < 2)(1)(2)
    ag.repeat().take(4).toList shouldBe List(
      0 -> Next(1, Next(true,Rep(0,Empty()),Val(true)),Next(1,Val(1),Next(1,Val(2),Val(1)))),
      1 -> Next(1, Next(true,Rep(1,Val(1)),Val(true)),Next(1,Val(1),Next(1,Val(2),Val(1)))),
      2 -> Next(2, Next(false, Rep(2, Val(2)), Val(false)), Next(2, Val(1), Next(2, Val(2), Val(2)))),
      3 -> Next(2, Next(false, Rep(3, Val(3)), Val(false)), Next(2, Val(1), Next(2, Val(2), Val(2))))
    )
    // could also test a match
    ag.repeat().take(4).toList match
      case List(0 -> Next(1, _, _), 1 -> Next(1, _, _), 2 -> Next(2, _, _), 3 -> Next(2, _, _)) =>
    ag.repeat().take(1).toList match
      case List(0 -> Next(res, Next(cond, _, Val(true)), Next(1, branch1, Next(1, branch2, Val(res2))))) =>

  test("Muxing with a rep, checking reps inside"):
    import AdditionalConstructs.*
    val ag = mux(for c <- rep(0)(_ + 1) yield c < 2 || c > 3)(counter(0))(100)
    ag.repeat().take(8).toList.map:
      case _ -> Next(v, _, _) => v
    .shouldBe(List(0, 1, 100, 100, 4, 5, 6, 7))

  test("Simple branch"):
    import AdditionalConstructs.*
    branch(true)("a")("b").repeat().take(2).toList match
      case List(0 -> Call(f, _, Val("a")), 1 -> Call(g, _, Val("a"))) if f == g =>
    branch(false)("a")("b").repeat().take(2).toList match
      case List(0 -> Call(f, _, Val("b")), 1 -> Call(g, _, Val("b"))) if f == g =>

  test("Fixed branch with counter"):
    import AdditionalConstructs.*
    val ag = branch(true)(counter(0))(100)
    ag.repeat().take(2).toList match
      case List(
        0 -> Call(_, Next(_, Val(true), _), Rep(0, Empty())),
        1 -> Call(_, Next(_, Val(true), _), Rep(1, Val(1))),
      ) =>

  test("Branching with a rep, checking reps inside"):
    import AdditionalConstructs.*
    val ag = branch(for c <- rep(0)(_ + 1) yield c < 2 || c > 3)(counter(0))(counter(100))
    ag.repeat().take(8).toList.map:
      case _ -> Call(_, _, res) => res.top
    .shouldBe(List(0, 1, 100, 101, 0, 1, 2, 3))

  /*
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

*/