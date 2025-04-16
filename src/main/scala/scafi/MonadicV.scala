package scafi

object MonadicV:

  case class Vect[A](as: A*):
    override def toString: String = as.toList.toString()

    def flatMap[B](f: A => Vect[B]): Vect[B] =
      Vect(as.zipWithIndex.map((a, i) => f(as(i)).as(i)):_*)

    def map[B](f: A => B): Vect[B] = Vect(as.map(f):_*)

  @main def playVs =
    val v: Vect[Int] = for
      b <- Vect(true, false, true)
      i <- Vect(10, 20, 30)
      j <- Vect(100, 200, 300)
    yield if b then i else j
    println(v)
