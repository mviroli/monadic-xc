package scafi

object AggregateFramework extends App:
  export Aggregate.{*, given}
  export Lib.*
  export Tree.*

  enum Tree[A]:
    case Rep(res: A, nest: Tree[A])
    case Val(res: A)
    case Next[A, B](res: A, left: Tree[B], right: Tree[A]) extends Tree[A]
    case Call(fun: () => Any, outer: Tree[() => Aggregate[A]], nest: Tree[A])
    case Empty()

    def top: A = this match
      case Rep(a, _) => a
      case Val(a) => a
      case Next(a, _, _) => a
      case Call(_, _, n) => n.top

  object Tree:
    def functionEquality(f1: Any, f2: Any): Boolean = f2 == f2

  trait Aggregate[A]:
    given Set[Device] = Set()

    def flatMap[B](f: A => Aggregate[B]): Aggregate[B] = Aggregate:
      case (Next(_, t1, t2), domain) =>
        val t1o = this.eval(t1.asInstanceOf[Tree[A]])(using domain)
        val t2o = f(t1o.top).eval(t2)(using domain)
        Next(t2o.top, t1o, t2o)
      case (Empty(), domain) =>
        val t1o = this.eval(Empty[A]())(using domain)
        val t2o = f(t1o.top).eval(Empty())(using domain)
        Next(t2o.top, t1o, t2o)

    def map[B](f: A => B): Aggregate[B] = flatMap(a => f(a))

    def eval(input: Tree[A])(using domain: Set[Device]): Tree[A]

  object Aggregate:
    opaque type Device = Int
    val selfDevice: Device = 0

    def apply[A](round: (Tree[A], Set[Device]) => Tree[A]): Aggregate[A] = new Aggregate[A]:
      override def eval(input: Tree[A])(using domain: Set[Device]): Tree[A] = round(input, domain)

    def compute[A](a: A): Aggregate[A] = Aggregate:
      (_, _) => Val(a)

    def rep[A](a: A)(f: A => Aggregate[A]): Aggregate[A] = Aggregate:
      case (Empty(), _) => Rep(a, Empty())
      case (_, domain) if !domain.contains(selfDevice) => Rep(a, Empty())
      case (Rep(x, nest), domain) => val n = f(x).eval(nest)(using domain); Rep(n.top, n)

    def aggregateCall[A](f: Aggregate[() => Aggregate[A]]): Aggregate[A] =
      Aggregate:
        case (Empty(), domain) =>
          val funTree = f.eval(Empty())(using domain)
          Call(funTree.top, funTree, funTree.top().eval(Empty())(using domain))
        case (Call(g, outer, nest), domain) =>
          val funTree = f.eval(outer.asInstanceOf[Tree[() => Aggregate[A]]])(using domain)
          if g == funTree.top
            then Call(g, funTree, funTree.top().eval(nest)(using domain))
            else Call(funTree.top, funTree, funTree.top().eval(Empty())(using domain - selfDevice))

    given [A]: Conversion[A, Aggregate[A]] = a => compute(a)

  object AdditionalConstructs:
    def nbr[A](a: Aggregate[A]): Aggregate[A] = ???

    def hood[A](initial: A, op: (A, A) => A)(a: Aggregate[A]): Aggregate[A] = ???

    def nbr_range: Aggregate[Double] = ???

    extension [A](a: Aggregate[A]) def >>[B](f: A => Aggregate[B]): Aggregate[B] = ???

    def mux[A](b: Aggregate[Boolean])(th: Aggregate[A])(el: Aggregate[A]): Aggregate[A] =
      for
        cond <- b
        t <- th
        e <- el
      yield if cond then t else e

    def branch[A](cond: Aggregate[Boolean])(th: Aggregate[A])(el: Aggregate[A]): Aggregate[A] =
      aggregateCall:
        mux(cond)(compute(() => th))(compute(() => el))

  object Showcase:

    import AdditionalConstructs.{*, given}

    def gradient(src: Boolean): Aggregate[Double] =

      mux(src)(compute(0.0)):
        rep(Double.PositiveInfinity): d =>
          hood[Double](Double.PositiveInfinity, _ max _):
            for
              d1 <- nbr_range
              d2 <- nbr(d)
            yield d1 + d2


  object Lib:

    def constant[A](c: A): Aggregate[A] =
      rep(c)(identity)

    def counter(from: Int): Aggregate[Int] =
      rep(from)(n => n + 1)

    def counterWithNesting(from: Int): Aggregate[Int] =
      rep(from)(n => for c <- rep(1)(identity) yield c + n)

  object FancyLib:

    import AdditionalConstructs.*

    def counterWithNesting2(from: Int): Aggregate[Int] =
      rep(from): n =>
        for c <- rep(1)(identity)
            c2 <- rep(n)(identity)
            c3 <- rep(0)(_ + 1)
        yield c + c2 + c3

    def counterWithNesting3(from: Int): Aggregate[Int] =
      rep(from): n =>
        rep(1)(identity).flatMap: c =>
          rep(n)(identity).flatMap: c2 =>
            rep(0)(_ + 1).flatMap: c3 =>
              c + c2 + c3


