package scafi

object AggregateFramework extends App:
  export Aggregate.{*, given}
  export Lib.*
  export Tree.*

  enum Tree[+A]:
    case Rep(res: A, nest: Tree[A])
    case Val(res: A)
    case Next(left: Tree[Any], right: Tree[A])
    case Call(fun: () => Any, nest: Tree[A])
    case Empty()

    def top: A = this match
      case Rep(a, _) => a
      case Val(a) => a
      case Next(_, r) => r.top
      case Call(_, n) => n.top

  object Tree:
    def functionEquality(f1: Any, f2: Any): Boolean = f2 == f2

  trait Aggregate[A]:
    given Set[Device] = Set()

    def flatMap[B](f: A => Aggregate[B]): Aggregate[B] = Aggregate:
      case (Next(t1, t2), domain) =>
        val t1o = this.eval(t1.asInstanceOf[Tree[A]])(using domain)
        Next(t1o, f(t1o.top).eval(t2)(using domain))
      case (Empty(), domain) =>
        val t1o = this.eval(Empty[A]())(using domain)
        Next(t1o, f(t1o.top).eval(Empty())(using domain))

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
      val fun = f.eval(Empty())(using Set()).top
      Aggregate:
        case (Empty(), domain) => Call(fun, fun().eval(Empty())(using domain))
        case (Call(g, nest), domain) if g == fun => Call(fun, fun().eval(nest)(using domain))
        case (Call(_, nest), domain) => Call(fun, fun().eval(Empty())(using domain - selfDevice))

    given [A]: Conversion[A, Aggregate[A]] = a => compute(a)

  object AdditionalConstructs:
    def nbr[A](a: Aggregate[A]): Aggregate[A] = ???

    def hood[A](initial: A, op: (A, A) => A)(a: Aggregate[A]): Aggregate[A] = ???

    def mux[A](b: Aggregate[Boolean])(th: Aggregate[A])(el: Aggregate[A]): Aggregate[A] = ???

    def nbr_range: Aggregate[Double] = ???

    def bind[A, B, C](s1: Aggregate[A], s2: Aggregate[B])(f: (A, B) => C): Aggregate[C] = ???

    def bind[A, B, C, D](s1: Aggregate[A], s2: Aggregate[B], s3: Aggregate[C])(f: (A, B, C) => D): Aggregate[D] = ???

    extension [A](a: Aggregate[A]) def >>[B](f: A => Aggregate[B]): Aggregate[B] = ???

    def branch[A](cond: Aggregate[Boolean])(th: Aggregate[A])(el: Aggregate[A]): Aggregate[A] =
      aggregateCall:
        for
          b <- cond
          f1 <- compute(() => th)
          f2 <- compute(() => el)
        yield if b then f1 else f2

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

    def counterWithNesting4(from: Int): Aggregate[Int] =
      rep(from): n =>
        bind(rep(1)(identity), rep(n)(identity), rep(0)(_ + 1)): (c, c2, c3) =>
          c + c2 + c3
