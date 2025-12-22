package scafi.lib

/**
 * A very minimal lib, mainly to showcase the API, but also including core
 * constructs (rep, retsend, mux, branch)
 */

object AggregateLib:
  import scafi.facade.AggregateLanguageModule.{*, given}

  def retsend[A](a: Aggregate[A])(f: NValue[A] => Aggregate[A]): Aggregate[A] =
    exchange(a)(v => (f(v), f(v)))

  def rep[A](a: Aggregate[A])(f: NValue[A] => Aggregate[A]): Aggregate[A] =
    retsend(a)(x => f(nself(x)))

  def nbr[A](a: Aggregate[A]): Aggregate[A] =
    exchange(a)(v => (v, a))

  def counter(initial: Int) =
    rep(initial)(for i <- _ yield i + 1)

  def self[A](ag: Aggregate[A]): Aggregate[A] =
    for v <- ag yield nself(v)

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
        nfold(Int.MaxValue)(_ min _):
          v.map(n => if n == Int.MaxValue then n else n + 1)

  def gradient(src: Aggregate[Boolean], distance: Aggregate[Double]): Aggregate[Double] =
    retsend(Double.PositiveInfinity): v =>
      mux(src)(0.0):
        for
          d <- distance
        yield nfold(Double.PositiveInfinity)(_ min _):
          for
            range <- d
            current <- v
          yield current + range


  extension [A](dv: (Double, A)) def min(dv2: (Double, A)): (Double, A) = if dv._1 < dv2._1 then dv else dv2

  def gradcast[A](src: Aggregate[Boolean], distance: Aggregate[Double])(field: Aggregate[A]): Aggregate[A] =
    retsend(field.map(_.map(Double.PositiveInfinity -> _))): v =>
      mux(src)(field.map(_.map(Double.PositiveInfinity -> _))):
        for
          fv <- field
          dv <- distance
        yield
          nfold(Double.PositiveInfinity -> fv.selfValue)(_ min _):
            for
              range <- dv
              current <- v.map(_._1)
              value <- v.map(_._2)
            yield current + range -> value
    .map(_.map(_._2))

