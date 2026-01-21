package scafi.facadeMonadXC

/**
 * A very minimal lib, mainly to showcase the API, but also including core
 * constructs (rep, retsend, mux, branch)
 */

object AggregateApplicativeSyntax:
  import fplib.Applicatives.*
  import scafi.facadeMonadXC.AggregateLanguageModule.{*, given}

  given Applicative[XC] with
    def pure[A](a: A): XC[A] = compute(a)

    def ap[A, B](ff: XC[A => B])(fa: XC[A]): XC[B] =
      for
        ffNV <- ff
        faNV <- fa
      yield for
        ffV <- ffNV
        faV <- faNV
      yield ffV(faV)

  extension [T <: Tuple, Z](t: T)

    def aMapN(f: Tuple.InverseMap[T, XC] => Z) =
      t.mapN[XC](f)

  extension [A](a: XC[A])
    def aMapN[B](f: A => B) =
      a.map(_.map(f))
