package scafi

import scala.reflect.ClassTag

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