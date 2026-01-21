package scafi.facadeMonadXC

/**
 * A very minimal lib, mainly to showcase the API, but also including core
 * constructs (rep, retsend, mux, branch)
 */

object AggregateLib:
  import scafi.facadeMonadXC.AggregateLanguageModule.{*, given}
  import AggregateApplicativeSyntax.*

  def retsend[A](a: Aggregate[A])(f: Aggregate[A] => Aggregate[A]): Aggregate[A] =
    exchange(a)(v => (f(v), f(v)))

  def rep[A](a: Aggregate[A])(f: Aggregate[A] => Aggregate[A]): Aggregate[A] =
    retsend(a)(x => f(self(x)))

  def nbr[A](a: Aggregate[A]): Aggregate[A] =
    exchange(a)(v => (v, a))

  def counter(initial: Int) =
    rep(initial)(_.aMapN(_+1))

  def mux[A](b: Aggregate[Boolean])(th: Aggregate[A])(el: Aggregate[A]): Aggregate[A] =
    for
      cond <- b
      t <- th
      e <- el
    yield if cond.selfValue then t else e

  def branch[A](cond: Aggregate[Boolean])(th: Aggregate[A])(el: Aggregate[A]): Aggregate[A] =
    call:
      mux(cond)(() => th)(() => el)

  def hopGradient(src: Aggregate[Boolean]): Aggregate[Int] =
    retsend(Int.MaxValue): v =>
      mux(src)(0):
        fold(Int.MaxValue)(_ min _):
          v.aMapN(n => if n == Int.MaxValue then n else n + 1)

  def gradient(src: Aggregate[Boolean])(using range: Aggregate[Double]): Aggregate[Double] =
    retsend(Double.PositiveInfinity): v =>
      mux(src)(0.0):
        fold(Double.PositiveInfinity)(_ min _):
          (range, v).aMapN(_ + _)


  def broadcast[A](src: Aggregate[Boolean])(field: Aggregate[A])(using range: Aggregate[Double]): Aggregate[(Double, A)] =
    extension [A](dv: (Double, A))
      def min(dv2: (Double, A)): (Double, A) = if dv._1 < dv2._1 then dv else dv2
    retsend(field.aMapN(Double.PositiveInfinity -> _)): dv =>
      mux(src)(field.aMapN(0.0 -> _)):
        fold(field.aMapN(Double.PositiveInfinity -> _))(_ min _):
          (dv, range).aMapN:
            case (distval, rng) => distval._1 + rng -> distval._2

  def distanceBetween(src: Aggregate[Boolean], dest: Aggregate[Boolean])(using range: Aggregate[Double]): Aggregate[Double] =
    broadcast(src)(gradient(dest)).aMapN(_._2)

  def channel(src: Aggregate[Boolean], dest: Aggregate[Boolean], width: Aggregate[Double])(using range: Aggregate[Double]): Aggregate[Boolean] =
    (gradient(src), gradient(dest), distanceBetween(src, dest), width).aMapN:
      case (gs, gd, d, w) => gs + gd - d < w
