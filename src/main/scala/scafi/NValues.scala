package scafi

object NValues:

  import Devices.{*, given}
  import NValue.given
  export NValue.*

  case class NValue[+A](a: A, map: Map[Device, A]):
    override def toString: String = a.toString + "[" + map.mkString(", ") + "]"
    def flatMap[B](f: A => NValue[B]): NValue[B] =
      NValue(f(a).a, (map.keySet ++ f(a).map.keySet).map(k => (k, f(this(k))(k))).toMap)
    def map[B](f: A => B): NValue[B] = NValue(f(this.a), map.map((k, v) => (k, f(v))))
    def apply(d: Device): A = map.getOrElse(d, a)
    def self: A = apply(selfDevice)
    def restrict(dom: Domain): NValue[A] = NValue(a, map.filter((k, _) => dom.contains(k)))
    def cut(dom: Domain): NValue[A] = NValue(a, map.filterNot((k, _) => dom.contains(k)))
    def map2[B, C](other: NValue[B])(f: (A, B) => C): NValue[C] =
      NValue(f(this.a, other.a), (this.map.keySet ++ other.map.keySet).map(k => (k, f(this(k),other(k)))).toMap)

  object NValue:
    given g[A]: Conversion[A, NValue[A]] = NValue(_, Map.empty)
    extension [A](a: A) def |>(e: (Device, A)*): NValue[A] =
      NValue(a, e.toMap)

/*
object AggregateAttempts:

  trait NValue[A]:
    def flatMap[B](f: A => NValue[B]): NValue[B] = ???
    def map[B](f: A => B): NValue[B] = ???
  given [A]: Conversion[A, NValue[A]] = ???

  trait Aggregate[A]:
    def flatMap[B](f: NValue[A] => Aggregate[B]): Aggregate[A] = ???
    def map[B](f: NValue[A] => NValue[B]): Aggregate[B] = ???

  object Aggregate:
    given [A]: Conversion[A, Aggregate[A]] = compute
    given [A]: Conversion[NValue[A], Aggregate[A]] = compute

    def compute[A](a: NValue[A]): Aggregate[A] = ???
    def exchange[A](a: NValue[A])(f: NValue[A] => (Aggregate[A], Aggregate[A])): Aggregate[A] = ???
    def rep[A](a: NValue[A])(f: NValue[A] => Aggregate[A]): Aggregate[A] = ???
    def call[A](f: Aggregate[() => Aggregate[A]]): Aggregate[A] = ???

  @main def tryNAFO =
    import Aggregate.{*, given}
    val f = () => rep(0)(n => for vn <- n yield vn + 1)
    // all typed Aggregate[Int]
    val ag = rep(0)(n => for vn <- n yield vn + 1)
    val ag2 = call(f)
    val ag3 = call(rep(f)(identity))
    val ag4 = for
      v1 <- exchange(0)(n => (n,n))
      v2 <- exchange(0)(n => (rep(0)(identity),n))
    yield for
      iv1 <- v1
      iv2 <- v2
    yield iv1 + iv2

end AggregateAttempts

object AggregateFOAttempts:

  trait NbrField[A]:
    def flatMap[B](f: A => NbrField[B]): NbrField[A] = ???
    def map[B](f: A => B): NbrField[B] = ???

  trait Aggregate[A]:
    def flatMap[B](f: A => Aggregate[B]): Aggregate[A] = ???
    def map[B](f: A => B): Aggregate[B] = ???

  object Aggregate:
    given [A]: Conversion[A, Aggregate[A]] = compute

    def compute[A](a: A): Aggregate[A] = ???
    def rep[A](a: A)(f: A => Aggregate[A]): Aggregate[A] = ???
    def branch[A](b: Aggregate[Boolean])(th: Aggregate[A])(el: Aggregate[A]): Aggregate[A] = ???
    def mux[A](b: Aggregate[Boolean])(th: Aggregate[A])(el: Aggregate[A]): Aggregate[A] = ???
    def nbr[A](a: Aggregate[A]): NbrField[A] = ???
    def fold[A](z: A)(o: (A, A) => A)(n: NbrField[A]): Aggregate[A] = ???

  @main def tryFONA =
    import Aggregate.{*, given}
    def cond: Boolean = ???
    def nbrRange: NbrField[Double] = ???
    val grad = rep[Double](Double.PositiveInfinity):d =>
      mux(cond)(0.0):
        fold(Double.PositiveInfinity)(_ min _):
          for
            d1 <- nbr(d)
            d2 <- nbrRange
          yield d1 + d2

end AggregateFOAttempts

*/