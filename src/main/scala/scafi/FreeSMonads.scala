package scafi

object FreeSMonads:
  trait SMonad[M[_], C[_]]:
    def pure[A](a: C[A]): M[A]
    def flatMap[A, B](ma: M[A])(f: C[A] => M[B]): M[B]

  object SMonad:
    def apply[M[_], C[_]](using stackableMonad: SMonad[M,C]): SMonad[M,C] = stackableMonad

  trait ~~>[F[_], G[_]]:
    def apply[A]: F[A] => G[A]

  trait FreeS[M[_], C[_], A]:
    import FreeS.*
    def flatMap[B](f: C[A] => FreeS[M, C, B]): FreeS[M, C, B] = FlatMap(this, f)
    def map[B](f: C[A] => C[B]): FreeS[M, C, B] = flatMap(a => pure(f(a)))
    def foldMap[G[_]](natTrans: M ~~> G)(using SMonad[G, C]): G[A] = this match
      case Pure(a) => SMonad[G, C].pure(a)
      case Suspend(ma) => natTrans.apply(ma)
      case FlatMap(fa, f) =>
        SMonad[G, C].flatMap(fa.foldMap(natTrans))(a => f(a).foldMap(natTrans))

  object FreeS:
    def pure[M[_], C[_], A](a: C[A]): FreeS[M, C, A] = Pure(a)
    def liftM[M[_], C[_], A](ma: M[A]): FreeS[M, C, A] = Suspend(ma)
    case class Pure[M[_], C[_], A](a: C[A]) extends FreeS[M, C, A]
    case class FlatMap[M[_], C[_], A, B](fa: FreeS[M, C, A], f: C[A] => FreeS[M, C, B]) extends FreeS[M, C, B]
    case class Suspend[M[_], C[_], A](ma: M[A]) extends FreeS[M, C, A]

  type Monad[M[_]] = SMonad[M, [X] =>> X]

  given Monad[[X] =>> X] with
    override def pure[A](a: A): A = a
    override def flatMap[A, B](ma: A)(f: A => B): B = f(ma)

  object Monad:
    def apply[M[_]](using monad: Monad[M]): Monad[M] = monad

  type Free[M[_], A] = FreeS[M, [X] =>> X, A]
