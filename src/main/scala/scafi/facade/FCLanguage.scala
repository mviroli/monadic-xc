package scafi.facade

import smonads.SMonads.*

trait FCLanguage:
  type Field[_]
  given monadField: Monad[Field]

  type NbrMap[_]
  given monadNbrMap: Monad[NbrMap]

  type NbrField[_]
  given monadNbrField: Monad[NbrField]

  given fieldFromValue[A]: Conversion[A, Field[A]]
  given nbrFromValue[A]: Conversion[A, NbrField[A]]

  def sensor[A](a: => A): Field[A]
  def nbrSensor[A](a: => NbrMap[A]): NbrField[A]

  def rep[A](a: =>A)(f: A => Field[A]): Field[A]
  def nbr[A](a: Field[A]): NbrField[A]
  def fold[A](init: A)(op: (A, A) => A)(nbr: NbrField[A]): Field[A]
  extension [A](f: Field[A]) def asValue: A

def play(fc: FCLanguage) =
  import fc.{*, given}
  def nbrRange: NbrField[Double] = ???

  def mux[A](cond: Field[Boolean])(th: Field[A])(el: Field[A]): Field[A] =
    for
      c <- cond
      t <- th
      e <- el
    yield (if cond.asValue then t else e)

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
      



