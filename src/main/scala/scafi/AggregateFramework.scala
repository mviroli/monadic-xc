package scafi

object AggregateFramework:
  export Aggregate.{*, given}
  export Lib.*
  export Tree.*

  enum Tree[A]:
    case Rep(res: A, nest: Tree[A])
    case Val(res: A)
    case Next[B, A](left: Tree[B], right: Tree[A]) extends Tree[A]
    case Call(fun: Tree[() => Aggregate[A]], nest: Tree[A])
    case Empty()

    def top: A = this match
      case Rep(a, _) => a
      case Val(a) => a
      case Next(_, r) => r.top
      case Call( _, n) => n.top

  opaque type Device = Int
  val selfDevice: Device = 0
  type Domain = Set[Device]

  trait Aggregate[A]:
    def eval(t: Tree[A])(using d: Domain): Tree[A]

  object Aggregate:

    private def aggregate[A](onEmpty: Tree[A])(round: Domain ?=> Tree[A] => Tree[A]): Aggregate[A] = new Aggregate[A]:
      override def eval(input: Tree[A])(using d: Domain): Tree[A] =
        round(if input == Empty() then onEmpty else input)

    extension [A](ag: Aggregate[A])
      def flatMap[B](f: A => Aggregate[B]): Aggregate[B] = aggregate(Next(Empty(), Empty())):
        case Next[A, B](left, right) =>
          val l2 = ag.eval(left)
          Next(l2, f(l2.top).eval(right))
      def map[B](f: A => B): Aggregate[B] = flatMap(a => compute(f(a)))

    def rep[A](a: A)(f: A => Aggregate[A]): Aggregate[A] = aggregate(Empty()):
      case Rep(x, nest) if summon[Domain].contains(selfDevice) => val n2 = f(x).eval(nest); Rep(n2.top, n2)
      case _ => Rep(a, Empty())

    def call[A](f: Aggregate[() => Aggregate[A]]): Aggregate[A] = aggregate(Call(Empty(), Empty())):
        case Call(fun, nest) =>
          val funOut = f.eval(fun)
          if fun != Empty() && fun.top == funOut.top
            then Call(funOut, funOut.top().eval(nest))
            else Call(funOut, funOut.top().eval(Empty())(using summon[Domain] - selfDevice))

    given compute[A]: Conversion[A, Aggregate[A]] = a => aggregate(Empty())(_ => Val(a))

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
      call:
        mux(cond)(compute(() => th))(compute(() => el))

    def gradient(src: Boolean): Aggregate[Double] =

        mux(src)(compute(0.0)):
          rep(Double.PositiveInfinity): d =>
            hood[Double](Double.PositiveInfinity, _ max _):
              for
                d1 <- nbr_range
                d2 <- nbr(d)
              yield d1 + d2

  object Showcase:

    import AdditionalConstructs.{*, given}




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


