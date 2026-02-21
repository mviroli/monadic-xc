package monadicXc

object Monads:
  trait Monad[M[_]]:
    def pure[A](a: A): M[A]
    extension [A](ma: M[A])
      def flatMap[B](f: A => M[B]): M[B]
      def map[B](f: A => B): M[B] = ma.flatMap(ca => pure(f(ca)))