package scafi.experiments

import smonads.SMonads.*

trait FCLanguage:
  type Field[_]
  given monadField: Monad[Field]

  type NbrField[_]
  given monadNbrField: Monad[NbrField]

  given fieldFromValue[A]: Conversion[A, Field[A]]
  given nbrFromValue[A]: Conversion[A, NbrField[A]]

  def sensor[A](a: => A): Field[A]

  type NbrMap[_]
  def nbrSensor[A](a: => NbrMap[A]): NbrField[A]

  def rep[A](a: =>A)(f: A => Field[A]): Field[A]
  def nbr[A](a: Field[A]): NbrField[A]
  def fold[A](init: A)(op: (A, A) => A)(nbr: NbrField[A]): Field[A]
  def branch[A](cond: Field[Boolean])(th: Field[A])(el: Field[A]): Field[A]

trait FCLanguageImpl extends FCLanguage:
  import scafi.facade.{AggregateLanguageModule => ALM}

  type Field[A] = ALM.Aggregate[A]
  given monadField: Monad[Field] with
    def pure[A](a: A): Field[A] = ALM.fromValue(a)
    extension [A](ma: Field[A]) def flatMap[B](f: A => Field[B]): Field[B] =
      ALM.monadAggregate.flatMap(ma)(nv => f(ALM.selfValue(nv))



def play(fc: FCLanguage) =
  import fc.{*, given}
  def nbrRange: NbrField[Double] = ???

  def mux[A](cond: Field[Boolean])(th: Field[A])(el: Field[A]): Field[A] =
    for
      c <- cond
      t <- th
      e <- el
    yield if c then t else e

  def gradient(src: Field[Boolean]): Field[Double] =
    rep(Double.PositiveInfinity): d =>
      mux[Double](src)(0.0):
        fold(Double.PositiveInfinity)(_ min _):
          for
            dd <- nbr(d)
            md <- nbrRange
          yield dd + md

trait Attempt:
  type Field[_]
  given m1: Monad[Field]
  type NValue[_]
  given m2: Monad[NValue]
  type NField[A] = Field[NValue[A]]
  def nf[A](a: A): NField[A]

def playAttempt(a: Attempt) =
  import a.*

  def compose =
    for
      v1 <- nf(5)
      v2 <- nf(6)
    yield for
      a1 <- v1
      a2 <- v2
    yield a1 + a2


def directLists =
  trait Ctx[T[_]]
  def element[T[_], A](using Ctx[T])(a: T[A]): A = ???
  def give[T[_], A](using Ctx[T])(a: =>A): A = ???

  def compose[T[_], A](body: Ctx[T] ?=> Unit) = ???
  given Ctx[List] = ???

  compose[List, Int]:
    var e1 = element(List(10,20,30))
    var e2 = element(List(40, 50))
    give:
      e1 + e2

