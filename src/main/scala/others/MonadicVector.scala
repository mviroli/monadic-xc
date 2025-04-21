package others

object MonadicVector:

  case class VectorFlatMap[A, B](vfm: (A => B, Vector[A])):
    override def toString: String = vfm._2.toString
  given v[A]: Conversion[VectorFlatMap[A, A], Vector[A]] = _.vfm._2

  case class Vector[A](l: Seq[A]):
    def flatMap[B](f: A => VectorFlatMap[B, B]): VectorFlatMap[B, B] =
      VectorFlatMap( identity,
        Vector(l.zipWithIndex.map( (a, i) => {val VectorFlatMap(g, vec) = f(a); g(vec.l(i))})))

    def map[B](f: A => B): VectorFlatMap[A, B] = VectorFlatMap(f, this)

  @main def playFree =
    val v:Vector[Int] =
      for
        x <- Vector(Seq(10,20,30))
        y <- Vector(Seq(1,2,3))
        z <- Vector(Seq(100, 200, 300))
      yield x + y + z
    println(v)