package scafi


import FreeSMonads.*

trait AggregateModule:
  type Aggregate[A]
  type NValue[A]

  given NValueMonad: Monad[NValue]
  given AggregateMonad: SMonad[Aggregate, NValue]

  given g1[A]: Conversion[A, Aggregate[A]]
  given g2[A]: Conversion[NValue[A], Aggregate[A]]

  def sensor[A](a: => A): Aggregate[A]
  def compute[A](a: NValue[A]): Aggregate[A]
  def call[A](f: Aggregate[() => Aggregate[A]]): Aggregate[A]
  def exchange[A](a: Aggregate[A])(f: NValue[A] => (Aggregate[A], Aggregate[A])): Aggregate[A]

  given g[A]: Conversion[A, NValue[A]] = apply(_)
  def apply[A](a: A): NValue[A]

  def nself[A](a: NValue[A]): NValue[A]
  def nfold[A](init: A)(op: (A, A) => A)(a: NValue[A]): NValue[A]
  extension [A](nv: NValue[A]) def selfValue(a: A): Boolean

end AggregateModule