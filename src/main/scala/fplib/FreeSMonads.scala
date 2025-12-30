package fplib

object FreeSMonads:
  export SMonads.*

  trait ~~>[F[_], G[_]]:
    def apply[A]: F[A] => G[A]

  trait FreeS[F[_], C[_], A]:
    import FreeS.*
    def foldMap[G[_]](natTrans: F ~~> G)(using SMonad[G, C]): G[A] = this match
      case Pure(a) => summon[SMonad[G, C]].pure(a)
      case Suspend(ma) => natTrans.apply(ma)
      case FlatMap(fa, f) =>
        summon[SMonad[G, C]].flatMap(fa.foldMap(natTrans))(a => f(a).foldMap(natTrans))

  object FreeS:
    def pure[F[_], C[_], A](a: C[A]): FreeS[F, C, A] = Pure(a)
    def liftM[F[_], C[_], A](ma: F[A]): FreeS[F, C, A] = Suspend(ma)
    case class Pure[F[_], C[_], A](a: C[A]) extends FreeS[F, C, A]
    case class FlatMap[F[_], C[_], A, B](fa: FreeS[F, C, A], f: C[A] => FreeS[F, C, B]) extends FreeS[F, C, B]
    case class Suspend[F[_], C[_], A](ma: F[A]) extends FreeS[F, C, A]

  given smonadFromFreeS[F[_], C[_]]: SMonad[[A] =>> FreeS[F, C, A], C] with
    def pure[A](a: C[A]): FreeS[F, C, A] = FreeS.pure[F, C, A](a)
    extension [A](ma: FreeS[F, C, A])
      def flatMap[B](f: C[A] => FreeS[F, C, B]): FreeS[F, C, B] = FreeS.FlatMap(ma, f)

  type Free[M[_], A] = FreeS[M, [X] =>> X, A]