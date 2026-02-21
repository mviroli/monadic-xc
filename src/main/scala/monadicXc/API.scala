package monadicXc

import Monads.*

trait API:
  // self-contained definitions to write-down ergonomic libs
  type NValue[+A]
  given toNValue[A]: Conversion[A, NValue[A]]
  extension [A](nv: NValue[A])
    def nvMap[B](f: A => B): NValue[B]
  extension [A, B](nv2: (NValue[A], NValue[B]))
    def nvMap2[C](f: (A, B) => C): NValue[C]

  type Aggregate[A]
  given ma: Monad[Aggregate]
  given pureConversion[A]: Conversion[A, Aggregate[A]] = ma.pure

  def exchange[A](n: =>NValue[A])(f: NValue[A] => Aggregate[(NValue[A], NValue[A])]): Aggregate[NValue[A]]
  def branch[A](b: =>Boolean)(th: Aggregate[A])(el: Aggregate[A]): Aggregate[A]
  def fold[A](initial: =>A)(op: (A, A) => A)(v: =>NValue[A]): Aggregate[A]
  def self[A](a: =>NValue[A]): Aggregate[A]
  def sensor[A](a: () => A): Aggregate[A]


