package monadicXc

import Monads.*

trait MonadicFramework extends API with AbstractEngine with Devices:

  extension [A](nv: NValue[A])
    override def nvMap[B](f: A => B): NValue[B] = NValue(f(nv.a), nv.map.map((k, v) => (k,f(v))))
    def update(map: Map[Device, A]): NValue[A] = NValue(nv.a, nv.map ++ map)
  extension [A, B](nv2: (NValue[A], NValue[B]))
    override def nvMap2[C](f: (A, B) => C): NValue[C] =
      NValue(f(nv2._1.a, nv2._2.a),
        (nv2._1.map.keySet ++ nv2._2.map.keySet).map(device => (device, f(nv2._1(device), nv2._2(device)))).toMap)
  given toNValue[A]: Conversion[A, NValue[A]] = NValue(_)

  override opaque type Message = Seq[Path]
  override def emptyMessage(): Seq[Path] = Seq()
  case class Path(node: Node, message: Seq[Path])
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

  def exchange[A](n: => NValue[A])(f: NValue[A] => Aggregate[(NValue[A], NValue[A])]): Aggregate[NValue[A]] =
    AggregateImpl: dself =>
      c =>
        val c2v = c.view.mapValues { case Path(Node.XC(ni: NValue[A]), mi) :: mi2 => (ni, mi, mi2) }
        val nxc = n.update(c2v.mapValues( (ni, _, _) => ni(dself)).toMap)
        val (c3, (nr, ns), m3) = f(nxc).run(dself)(c2v.mapValues((_, mi, _) => mi).toMap)
          //println(s"$c ${(c2v.mapValues((_, _, mi2) => mi2).toMap, nr, Seq(Path(Node.XC(ns), m3)))}")
        (c2v.mapValues((_, _, mi2) => mi2).toMap, nr, Seq(Path(Node.XC(ns), m3)))

  def branch[A](b: =>Boolean)(th: Aggregate[A])(el: Aggregate[A]): Aggregate[A] =
    AggregateImpl: dself =>
      c =>
        val (_, n, m) = (if b then th else el).run(dself)(c.collect { case (dev, Path(Node.IF(bb), mi) :: _) if bb == b => (dev, mi) })
        (c.collect {case (dev, _ :: mi2) => (dev, mi2) }, n, Seq(Path(Node.IF(`b`), m)))
  def fold[A](initial: =>A)(op: (A, A) => A)(v: =>NValue[A]): Aggregate[A] = AggregateImpl:
    d => c =>
      (c, (c.keySet - d).toList.map(dev => v(dev)).fold(initial)(op), Seq())

  def self[A](a: =>NValue[A]): Aggregate[A] = AggregateImpl:
    d => c =>
      val (c1, a1, m1) = a.run(d)(c)
      (c1, a1(d), m1)

  def sensor[A](sns: () => A): Aggregate[A] =
    AggregateImpl(d => c => (c, sns(), Seq()))

