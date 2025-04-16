package scafi

object IndexedVectors:

  import scala.compiletime.constValue

  case class IxVec[N <: Int, A](vector: Vector[A])
    //require(vector.length == constValue[N], s"Length must be ${constValue[N]}")

  extension [N <: Int, A](v: IxVec[N, A])
    def map[B](f: A => B): IxVec[N, B] =
      IxVec(v.vector.map(f))

    def flatMap[B](f: A => IxVec[N, B]): IxVec[N, B] =
      // Apply f to each element, then zip the results element-wise
      val mapped = v.vector.map(f)
      val zipped = mapped
        .map(_.vector)
        .transpose
        .map(_.head) // All have same length, so safe
      IxVec(Vector.from(zipped))

  @main def playIndexed =
    import scala.compiletime.constValue
    val v: IxVec[_, Int] =
      for
        x <- IxVec(Vector(1,2,3))
        y <- IxVec(Vector(10,20,30))
      yield x + y
    println(v)