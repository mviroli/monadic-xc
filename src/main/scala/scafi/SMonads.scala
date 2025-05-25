package scafi

object SMonads:
  trait SMonad[M[_], C[_]]:
    def pure[A](a: C[A]): M[A]
    def flatMap[A, B](ma: M[A])(f: C[A] => M[B]): M[B]

  object SMonad:
    def apply[M[_], C[_]](using stackableMonad: SMonad[M, C]): SMonad[M, C] = stackableMonad

  type Monad[M[_]] = SMonad[M, [X] =>> X]

  given Monad[[X] =>> X] with
    override def pure[A](a: A): A = a
    override def flatMap[A, B](ma: A)(f: A => B): B = f(ma)

  object Monad:
    def apply[M[_]](using monad: Monad[M]): Monad[M] = monad
