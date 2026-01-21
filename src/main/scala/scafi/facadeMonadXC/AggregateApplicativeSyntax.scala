package scafi.facadeMonadXC

/**
 * A very minimal lib, mainly to showcase the API, but also including core
 * constructs (rep, retsend, mux, branch)
 */

object AggregateApplicativeSyntax:
  import fplib.Applicatives.*
  import scafi.facadeMonadXC.AggregateLanguageModule.{*, given}

  given Applicative[Aggregate] with
    def pure[A](a: A): Aggregate[A] = compute(a)

    def ap[A, B](ff: Aggregate[A => B])(fa: Aggregate[A]): Aggregate[B] =
      for
        ffNV <- ff
        faNV <- fa
      yield for
        ffV <- ffNV
        faV <- faNV
      yield ffV(faV)

  extension [T <: Tuple, Z](t: T)

    def aMapN(f: Tuple.InverseMap[T, Aggregate] => Z) =
      t.mapN[Aggregate](f)

  extension [A](a: Aggregate[A])
    def aMapN[B](f: A => B) =
      a.map(_.map(f))
