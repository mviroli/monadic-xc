package scafi.facadeMonadXC

/**
 * A very minimal lib, mainly to showcase the API, but also including core
 * constructs (rep, retsend, mux, branch)
 */

object AggregateLib:
  import scafi.facadeMonadXC.AggregateLanguageModule.{*, given}
  import AggregateApplicativeSyntax.*


  def retsend[A](a: XC[A])(f: XC[A] => XC[A]): XC[A] =
    exchange(a)(v => (f(v), f(v)))

  def rep[A](a: XC[A])(f: XC[A] => XC[A]): XC[A] =
    retsend(a)(x => f(self(x)))

  def nbr[A](a: XC[A]): XC[A] =
    exchange(a)(v => (v, a))

  def counter(initial: Int): XC[Int] =
    rep(compute(initial))(_.aMapN(_+1))

  def mux[A](b: XC[Boolean])(th: XC[A])(el: XC[A]): XC[A] =
    for
      cond <- b
      t <- th
      e <- el
    yield if cond.selfValue then t else e

  def branch[A](cond: XC[Boolean])(th: XC[A])(el: XC[A]): XC[A] =
    call:
      mux(cond)(() => th)(() => el)

  def hopGradient(src: XC[Boolean]): XC[Int] =
    retsend(Int.MaxValue): v =>
      mux(src)(0):
        fold(Int.MaxValue)(_ min _):
          v.aMapN(n => if n == Int.MaxValue then n else n + 1)

  def gradient(src: XC[Boolean])(using range: XC[Double]): XC[Double] =
    retsend(Double.PositiveInfinity): v =>
      mux(src)(0.0):
        fold(Double.PositiveInfinity)(_ min _):
          (range, v).aMapN(_ + _)


  def broadcast[A](src: XC[Boolean])(field: XC[A])(using range: XC[Double]): XC[(Double, A)] =
    extension [A](dv: (Double, A))
      def min(dv2: (Double, A)): (Double, A) = if dv._1 < dv2._1 then dv else dv2
    retsend(field.aMapN(Double.PositiveInfinity -> _)): dv =>
      mux(src)(field.aMapN(0.0 -> _)):
        fold(field.aMapN(Double.PositiveInfinity -> _))(_ min _):
          (dv, range).aMapN:
            case (distval, rng) => distval._1 + rng -> distval._2

  def distanceBetween(src: XC[Boolean], dest: XC[Boolean])(using range: XC[Double]): XC[Double] =
    broadcast(src)(gradient(dest)).aMapN(_._2)

  def channel(src: XC[Boolean], dest: XC[Boolean], width: XC[Double])(using range: XC[Double]): XC[Boolean] =
    (gradient(src), gradient(dest), distanceBetween(src, dest), width).aMapN:
      case (gs, gd, d, w) => gs + gd - d < w
