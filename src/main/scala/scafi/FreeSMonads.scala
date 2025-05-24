package scafi

object FreeSMonads:
  trait SMonad[M[_], C[_]]:
    def pure[A](a: C[A]): M[A]
    def flatMap[A, B](ma: M[A])(f: C[A] => M[B]): M[B]

  object SMonad:
    def apply[M[_], C[_]](using stackableMonad: SMonad[M,C]): SMonad[M,C] = stackableMonad

  trait ~~>[F[_], G[_]]:
    def apply[A]: F[A] => G[A]

  trait FreeS[F[_], C[_], A]:
    import FreeS.*
    def flatMap[B](f: C[A] => FreeS[F, C, B]): FreeS[F, C, B] = FlatMap(this, f)
    def map[B](f: C[A] => C[B]): FreeS[F, C, B] = flatMap(a => pure(f(a)))
    def foldMap[G[_]](natTrans: F ~~> G)(using SMonad[G, C]): G[A] = this match
      case Pure(a) => SMonad[G, C].pure(a)
      case Suspend(ma) => natTrans.apply(ma)
      case FlatMap(fa, f) =>
        SMonad[G, C].flatMap(fa.foldMap(natTrans))(a => f(a).foldMap(natTrans))

  object FreeS:
    def pure[F[_], C[_], A](a: C[A]): FreeS[F, C, A] = Pure(a)
    def liftM[F[_], C[_], A](ma: F[A]): FreeS[F, C, A] = Suspend(ma)
    case class Pure[F[_], C[_], A](a: C[A]) extends FreeS[F, C, A]
    case class FlatMap[F[_], C[_], A, B](fa: FreeS[F, C, A], f: C[A] => FreeS[F, C, B]) extends FreeS[F, C, B]
    case class Suspend[F[_], C[_], A](ma: F[A]) extends FreeS[F, C, A]

  type Monad[M[_]] = SMonad[M, [X] =>> X]

  given Monad[[X] =>> X] with
    override def pure[A](a: A): A = a
    override def flatMap[A, B](ma: A)(f: A => B): B = f(ma)

  object Monad:
    def apply[M[_]](using monad: Monad[M]): Monad[M] = monad

  type Free[M[_], A] = FreeS[M, [X] =>> X, A]
