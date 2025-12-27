package scafi.lib

/**
 * A very minimal lib, mainly to showcase the API, but also including core
 * constructs (rep, retsend, mux, branch)
 */

object Polyadic:
  import scafi.facade.AggregateLanguageModule.{*, given}

  trait Applicative[F[_]]:
    def pure[A](a: A): F[A]

    def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]

    def map[A, B](fa: F[A])(f: A => B): F[B] =
      ap(pure(f))(fa)

    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
      ap(ap(pure((a: A) => (b: B) => (a, b)))(fa))(fb)

  trait TupleApplicative[F[_], T <: Tuple]:
    def sequence(t: T)(using Applicative[F]): F[Tuple.InverseMap[T, F]]

  given emptyTupleApplicative[F[_]]: TupleApplicative[F, EmptyTuple] with
    def sequence(t: EmptyTuple)(using F: Applicative[F]): F[EmptyTuple] =
      F.pure(EmptyTuple)

  given consTupleApplicative[F[_], H, T <: Tuple](using
                                                  tailTA: TupleApplicative[F, T]
                                                 ): TupleApplicative[F, F[H] *: T] with

    def sequence(t: F[H] *: T)(using F: Applicative[F]): F[H *: Tuple.InverseMap[T, F]] =
      F.ap(F.ap(F.pure((hh: H) => (tt: Tuple.InverseMap[T, F]) => hh *: tt))(t.head))(tailTA.sequence(t.tail))

  def mapN[F[_], T <: Tuple, Z](t: T)(f: Tuple.InverseMap[T, F] => Z)(using
                                                 F: Applicative[F],
                                                 TA: TupleApplicative[F, T]
  ): F[Z] =
    F.map(TA.sequence(t))(f)

//  def mapN[F[_], T <: Tuple, Z](t: T)(f: Tuple.InverseMap[T, F] => Z)(using F: Applicative[F]): F[Z] = ???
    //val xs = t.toList.asInstanceOf[List[F[Any]]]
    //val vtup = xs.foldRight[Tuple](EmptyTuple)(_ *: _).asInstanceOf[Tuple.InverseMap[Tup, Option]]



  def map3[F[_], A, B, C, Z](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => Z)(using app: Applicative[F]) : F[Z] =
    import app.*
    ap(ap(ap(pure((a: A) => (b: B) => (c: C) => f(a,b,c)))(fa))(fb))(fc)

  @main def m =
    given Applicative[Option] with
      def pure[A](a: A): Option[A] = Some(a)
      def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] = ff.flatMap(f => fa.map(a => f(a)))

    println(map3(Option(1), Option(2), Option(3))(_ + _ > _))
    println(mapN[Option, Option[Int] *: Option[Int] *: Option[Boolean] *: EmptyTuple, Boolean](Option(1), Option(2), Option(true))((a: Int, b: Int, c: Boolean) => a > b && c))