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

    def unit[A](a: A): Vect[A] = Vect(a,a,a)
    println(unit(10).flatMap(x => Vect(x+1, x+2, x+3))) //Vect(11,12,13), prima legge ok
    println(Vect(10,20,30).flatMap(unit(_))) //Vect(10,20,30), seconda legge ok
    // i due sotto devono essere uguali per la terza legge, e lo sono
    println(Vect(10,20,30).flatMap(x => Vect(x+1,x+2,x+3)).flatMap(y => Vect(y, y*2, y*3))) // 11,44,99
    println(Vect(10, 20, 30).flatMap(x => Vect(x + 1, x + 2, x + 3).flatMap(y => Vect(y, y * 2, y * 3)))) // 11,44,99


