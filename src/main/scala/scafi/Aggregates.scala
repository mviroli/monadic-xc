package scafi

import scala.reflect.ClassTag

trait AggregateModule:
  type Aggregate[A]
  type NV[A]

  given g1[A]: Conversion[A, Aggregate[A]]
  given g2[A]: Conversion[NV[A], Aggregate[A]]

  def sensor[A](a: => A): Aggregate[A]
  def compute[A](a: NV[A]): Aggregate[A]
  def call[A](f: Aggregate[() => Aggregate[A]]): Aggregate[A]
  def exchange[A](a: Aggregate[A])(f: NV[A] => (Aggregate[A], Aggregate[A])): Aggregate[A]
end AggregateModule


object Aggregates:
  import CFreeMonads.*
  export NValues.{*, given}
  export Devices.*
  export Aggregate.{*, given}

  enum AggregateAST[A]:
    case Val(a: () => NValue[A])
    case Call(f: NValue[() => Aggregate[A]])
    case Xc(a: NValue[A], f: NValue[A] => (Aggregate[A], Aggregate[A]))

  import AggregateAST.*

  type Aggregate[A] = CFree[AggregateAST, NValue, A]

  object Aggregate:
    given [A]: Conversion[A, Aggregate[A]] = NValue.apply(_)
    given [A]: Conversion[NValue[A], Aggregate[A]] = compute(_)

    def sensor[A](a: =>A): Aggregate[A] = CFree.liftM(Val(() => NValue(a)))
    def compute[A](a: NValue[A]): Aggregate[A] = CFree.Pure(a)
    def call[A](f: Aggregate[() => Aggregate[A]]): Aggregate[A] = f.flatMap(v => CFree.liftM(Call(v)))
    def exchange[A](a: Aggregate[A])(f: NValue[A] => (Aggregate[A], Aggregate[A])): Aggregate[A] = a.flatMap(v => CFree.liftM(Xc(v, f)))
  export Semantics.*