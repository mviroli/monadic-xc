package scafi

object CFreeMonads:
  trait CMonad[M[_], C[_]]:
    def pure[A](a: C[A]): M[A]
    def flatMap[A, B](ma: M[A])(f: C[A] => M[B]): M[B]

  object CMonad:
    def apply[M[_], C[_]](using cmonad: CMonad[M,C]): CMonad[M,C] = cmonad

  trait ~~>[F[_], G[_]]:
    def apply[A]: F[A] => G[A]

  trait CFree[M[_], C[_], A]:

    import CFree.*

    def flatMap[B](f: C[A] => CFree[M, C, B]): CFree[M, C, B] = FlatMap(this, f)

    def map[B](f: C[A] => C[B]): CFree[M, C, B] = flatMap(a => pure(f(a)))

    def foldMap[G[_]](natTrans: M ~~> G)(using CMonad[G, C]): G[A] = this match
      case Pure(a) => CMonad[G, C].pure(a)
      case Suspend(ma) => natTrans.apply(ma)
      case FlatMap(fa, f) => // need a G[B]
        CMonad[G, C].flatMap(fa.foldMap(natTrans))(a => f(a).foldMap(natTrans))

  object CFree:
    def pure[M[_], C[_], A](a: C[A]): CFree[M, C, A] = Pure(a)
    def liftM[M[_], C[_], A](ma: M[A]): CFree[M, C, A] = Suspend(ma)
    case class Pure[M[_], C[_], A](a: C[A]) extends CFree[M, C, A]
    case class FlatMap[M[_], C[_], A, B](fa: CFree[M, C, A], f: C[A] => CFree[M, C, B]) extends CFree[M, C, B]
    case class Suspend[M[_], C[_], A](ma: M[A]) extends CFree[M, C, A]
