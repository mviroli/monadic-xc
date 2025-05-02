package others

import scafi.NValues
import scafi.NValues.NValue

object AggregatesNF:
  export Aggregate.*
  import NValues.{*, given}

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

@main def tryNA =
  import AggregatesNF.{*, given}
  val f = () => rep(0)(n => for vn <- n yield vn + 1)
  // all typed Aggregate[Int]
  val ag = rep(0)(n => for vn <- n yield vn + 1)
  val ag2 = call(f)
  val ag3 = call(rep(f)(identity))
  val ag4 = for
    v1 <- exchange[Int](0)(n => (n, n))
    v2 <- exchange[Int](0)(n => (rep(0)(identity), n))
  yield for
    iv1 <- v1
    iv2 <- v2
  yield iv1 + iv2
