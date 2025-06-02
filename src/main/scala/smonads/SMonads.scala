package smonads

object SMonads:
  trait SMonad[M[_], C[_]]:
    def pure[A](a: C[A]): M[A]
    extension [A](ma: M[A])
      def flatMap[B](f: C[A] => M[B]): M[B]
      def map[B](f: C[A] => C[B]): M[B] = ma.flatMap(ca => pure(f(ca)))

  type Monad[M[_]] = SMonad[M, [X] =>> X]

  given Monad[[X] =>> X] with
    override def pure[A](a: A): A = a
    extension [A](ma: A)
      override def flatMap[B](f: A => B): B = f(ma)

