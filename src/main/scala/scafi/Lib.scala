package scafi

object Lib:
  import Aggregates.*

  def counter: Aggregate[Int] = rep(0)(_ + 1)

  def counter(n: Int): Aggregate[Int] = rep(n)(_ + 1)

  def mux[A](b: Aggregate[Boolean])(th: Aggregate[A])(el: Aggregate[A]): Aggregate[A] =
    for
      cond <- b
      t <- th
      e <- el
    yield if cond then t else e

  def branch[A](cond: Aggregate[Boolean])(th: Aggregate[A])(el: Aggregate[A]): Aggregate[A] =
    call:
      mux(cond)(() => th)(() => el)


