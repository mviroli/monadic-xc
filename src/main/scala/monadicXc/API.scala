package monadicXc

object Monads:
  trait Monad[M[_]]:
    def pure[A](a: A): M[A]
    extension [A](ma: M[A])
      def flatMap[B](f: A => M[B]): M[B]
      def map[B](f: A => B): M[B] = ma.flatMap(ca => pure(f(ca)))
import Monads.*

trait API:
  // self-contained definitions to write-down ergonomic libs
  type NValue[+A]
  given toNValue[A]: Conversion[A, NValue[A]]
  extension [A](nv: NValue[A])
    def nvMap[B](f: A => B): NValue[B]
  extension [A, B](nv2: (NValue[A], NValue[B]))
    def nvMap2[C](f: (A, B) => C): NValue[C]

  type Aggregate[A]
  given ma: Monad[Aggregate]
  given pureConversion[A]: Conversion[A, Aggregate[A]] = ma.pure

  def exchange[A](n: NValue[A])(f: NValue[A] => Aggregate[(NValue[A], NValue[A])]): Aggregate[NValue[A]]
  def branch[A](b: Boolean)(th: Aggregate[A])(el: Aggregate[A]): Aggregate[A]
  def fold[A](initial: A)(op: (A, A) => A)(v: NValue[A]): Aggregate[A]
  def self[A](a: NValue[A]): Aggregate[A]

trait Engine:
  // engine abstract concepts
  type Device
  type NValue[+A] = PartialFunction[Device, A]
  type Message
  type Context = Map[Device, Message]

  trait Aggregate[A]:
    def run: Device => Context => (Context, A, Message)

object Framework extends Engine with API:
  // untested, very partial implementation

  override opaque type Device = Int
  extension [A](nv: NValue[A])
    override def nvMap[B](f: A => B): NValue[B] = d => f(nv(d))
  extension [A, B](nv2: (NValue[A], NValue[B]))
    override def nvMap2[C](f: (A, B) => C): NValue[C] = d => f(nv2._1(d), nv2._2(d))
  given toNValue[A]: Conversion[A, NValue[A]] = a => _ => a

  override opaque type Message = Seq[Path]
  trait Path:
    def node: Node
    def message: Message
  enum Node:
    case XC[A](nv: NValue[A])
    case IF(b: Boolean)

  case class AggregateImpl[A](run: Device => Context => (Context, A, Message)) extends Aggregate[A]

  given ma:Monad[Aggregate] with

    override def pure[A](a: A): Aggregate[A] =
      AggregateImpl(d => c => (c, a, Seq()))

    extension [A](ma: Aggregate[A])
      override def flatMap[B](f: A => Aggregate[B]): Aggregate[B] =
        AggregateImpl: d =>
          c =>
            val (c1, a1, m1) = ma.run(d)(c)
            val (c2, a2, m2) = f(a1).run(d)(c1)
            (c2, a2, m1 ++ m2)

  def exchange[A](n: NValue[A])(f: NValue[A] => Aggregate[(NValue[A], NValue[A])]): Aggregate[NValue[A]] = ???
  def branch[A](b: Boolean)(th: Aggregate[A])(el: Aggregate[A]): Aggregate[A] = ???
  def fold[A](initial: A)(op: (A, A) => A)(v: NValue[A]): Aggregate[A] = AggregateImpl:
    d => c =>
      (c, (c.keySet - d).toList.map(dev => v(dev)) .fold(initial)(op), Seq())

  def self[A](a: NValue[A]): Aggregate[A] = AggregateImpl:
    d => c =>
      val (c1, a1, m1) = a.run(d)(c)
      (c1, a1(d), m1)


object Lib:
  val api: API = Framework
  import api.{*, given}

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