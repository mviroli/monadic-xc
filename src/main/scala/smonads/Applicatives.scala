package smonads

object Applicatives:

  trait Applicative[F[_]]:
    def pure[A](a: A): F[A]

    def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]

    def map[A, B](fa: F[A])(f: A => B): F[B] =
      ap(pure(f))(fa)

    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
      ap(ap(pure((a: A) => (b: B) => (a, b)))(fa))(fb)


  extension [T <: Tuple, Z](t: T)
    def mapN[F[_]](f: Tuple.InverseMap[T, F] => Z)(using F: Applicative[F]): F[Z] =
      F.map(sequence(t).asInstanceOf[F[Tuple.InverseMap[T, F]]])(f)

  type Mapper[T <: Tuple, F[_]] = T match
    case EmptyTuple => F[EmptyTuple]
    case F[h] *: t => F[h *: Tuple.InverseMap[t, F]]
  private def sequence[T <: Tuple, F[_]](t: T)(using F: Applicative[F]): Mapper[T, F] = t match
    case EmptyTuple => F.pure(EmptyTuple).asInstanceOf[Mapper[T, F]]
    case h *: t2 =>
      F.ap(F.ap(F.pure((hh: Any) => (tt: Tuple.InverseMap[T, F]) => hh *: tt))(h.asInstanceOf[F[Any]]))(sequence(t2).asInstanceOf[F[Tuple.InverseMap[T, F]]]).asInstanceOf[Mapper[T,F]]

  @main def m =
    given Applicative[Option] with
      def pure[A](a: A): Option[A] = Some(a)
      def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] = ff.flatMap(f => fa.map(a => f(a)))

    println(sequence((Option(1), Option(2), Option(true))))
    println((Option(1), Option(2), Option(true)).mapN[Option](_ > _ && _))