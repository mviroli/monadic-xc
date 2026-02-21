package monadicXc

trait Lib:
  this: API =>

  //derived
  def retsend[A](a: Aggregate[A])(f: NValue[A] => Aggregate[A]): Aggregate[NValue[A]] =
    a.flatMap: nva =>
      exchange(nva)(nv => f(nv).map(a => (a,a)))


  def rep[A](a: A)(f: A => Aggregate[A]): Aggregate[A] =
    retsend(a)(n => self(n).flatMap(f)).flatMap(self(_))

  //examples
  def counter: Aggregate[Int] = rep(0)(_ + 1)

  def mux[A](b: Aggregate[Boolean])(th: Aggregate[A])(el: Aggregate[A]): Aggregate[A] =
    for
      cond <- b
      t <- th
      e <- el
    yield if cond then t else e

  def hopGradient(src: Aggregate[Boolean]): Aggregate[Int] =
    retsend(Int.MaxValue): v =>
      mux(src)(0):
        fold(Int.MaxValue)(_ min _):
          v.nvMap(n => if n == Int.MaxValue then n else n + 1)
    .flatMap(self(_))

  def gradient(src: Aggregate[Boolean])(using range: Aggregate[NValue[Double]]): Aggregate[Double] =
    retsend(Double.PositiveInfinity): distance =>
      mux(src)(0.0):
        for
          nvr <- range
          dd <- fold(Double.PositiveInfinity)(_ min _):
            (nvr, distance).nvMap2(_ + _)
        yield
          dd
    .flatMap(self(_))

  def broadcast[A](src: Aggregate[Boolean])(field: Aggregate[A])(using range: Aggregate[NValue[Double]]): Aggregate[(Double, A)] =
    extension [A](dv: (Double, A))
      def min(dv2: (Double, A)): (Double, A) = if dv._1 < dv2._1 then dv else dv2
    retsend(field.map(Double.PositiveInfinity -> _)): dv =>
      mux(src)(field.map(0.0 -> _)):
        for
          nvr <- range
          init <- field.map(Double.PositiveInfinity -> _)
          dd <- fold(init)(_ min _):
            (dv, nvr).nvMap2:
              case (distval, rng) => distval._1 + rng -> distval._2
        yield dd
    .flatMap(self(_))
  def distanceBetween(src: Aggregate[Boolean], dest: Aggregate[Boolean])(using range: Aggregate[NValue[Double]]): Aggregate[Double] =
    broadcast(src)(gradient(dest)).map(_._2)

  def channel(src: Aggregate[Boolean], dest: Aggregate[Boolean], width: Aggregate[Double])(using range: Aggregate[NValue[Double]]): Aggregate[Boolean] =
    for
      gs <- gradient(src)
      gd <- gradient(dest)
      d <- distanceBetween(src, dest)
      w <- width
    yield
      gs + gd - d < w
