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
        fold(Int.MaxValue)(_ min _):
          v.map(n => if n == Int.MaxValue then n else n + 1)

  def gradient(src: Aggregate[Boolean])(using range: Aggregate[Double]): Aggregate[Double] =
    retsend(Double.PositiveInfinity): v =>
      mux(src)(0.0):
        fold(Double.PositiveInfinity)(_ min _):
          for
            d <- range
          yield for
            range <- d
            current <- v
          yield current + range


  extension [A](dv: (Double, A)) def min(dv2: (Double, A)): (Double, A) = if dv._1 < dv2._1 then dv else dv2
  extension [A](a: Aggregate[A]) def deepMap[B](f: A => B): Aggregate[B] = a.map(_.map(f))

  def broadcast[A](src: Aggregate[Boolean])(field: Aggregate[A])(using range: Aggregate[Double]): Aggregate[(Double, A)] =
    retsend(field.deepMap(Double.PositiveInfinity -> _)): v =>
      mux(src)(field.deepMap(0.0 -> _)):
        fold(field.deepMap(Double.PositiveInfinity -> _))(_ min _):
          for
            fieldNV <- field
            distanceNV <- range
          yield for
              rangeV <- distanceNV
              distanceV <- v.map(_._1)
              fieldV <- v.map(_._2)
          yield distanceV + rangeV -> fieldV

  def gradcast[A](src: Aggregate[Boolean])(op: (A, Double) => A, field: Aggregate[A])(using range: Aggregate[Double]): Aggregate[(Double, A)] =
    retsend(field.deepMap(Double.PositiveInfinity -> _)): v =>
      mux(src)(field.deepMap(0.0 -> _)):
        fold(field.deepMap(Double.PositiveInfinity -> _))(_ min _):
          for
            fieldNV <- field
            distanceNV <- range
          yield for
            rangeV <- distanceNV
            distanceV <- v.map(_._1)
            fieldV <- v.map(_._2)
          yield distanceV + rangeV -> op(fieldV, distanceV + rangeV)

  def distanceBetween(src: Aggregate[Boolean], dest: Aggregate[Boolean])(using range: Aggregate[Double]): Aggregate[Double] =
    broadcast(src)(gradient(dest)).deepMap(_._2)

  def channel(src: Aggregate[Boolean], dest: Aggregate[Boolean], width: Aggregate[Double])(using range: Aggregate[Double]): Aggregate[Boolean] =
    for
      srcDistanceNV <- gradient(src)
      destDistanceNV <- gradient(dest)
      distanceBetweenNV <- distanceBetween(src, dest)
      widthNV <- width
    yield for
      srcDistanceV <- srcDistanceNV
      destDistanceV <- destDistanceNV
      distanceBetweenV <- distanceBetweenNV
      widthV <- widthNV
    yield srcDistanceV + destDistanceV - distanceBetweenV < widthV
/*
  case class DeepAggregate[A](a: Aggregate[A]):
    def flatMap[B](f: A => DeepAggregate[B]): DeepAggregate[B] =
      a.flatMap(nvx => nvx.flatMap(x => f(x).a.selfValue)).deep
    def map[B](f: A => B): DeepAggregate[B] = ???

  extension [A](a: Aggregate[A])
    def deep: DeepAggregate[A] = DeepAggregate(a)

  given f[A]:Conversion[DeepAggregate[A], Aggregate[A]] = d => d.a

  def channel2(src: Aggregate[Boolean], dest: Aggregate[Boolean], width: Aggregate[Double])(using range: Aggregate[Double]): Aggregate[Boolean] =
    for
      srcDistance <- gradient(src).deep
      destDistance <- gradient(dest).deep
      distanceBetween <- distanceBetween(src, dest).deep
      w <- width.deep
    yield srcDistance + destDistance - distanceBetween < w

    */