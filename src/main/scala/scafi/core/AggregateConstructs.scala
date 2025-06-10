package scafi.core

/**
 * DSL for Aggregates, using the FreeMonad approach
 */

object AggregateConstructs:
  import smonads.FreeSMonads.{*, given}
  export NValueConstructs.{*, given}
  export Aggregate.{*, given}

  enum AggregateAST[A]:
    case Val(a: () => NValue[A])
    case Call(f: NValue[() => Aggregate[A]])
    case Xc(a: NValue[A], f: NValue[A] => (Aggregate[A], Aggregate[A]))

  type Aggregate[A] = FreeS[AggregateAST, NValueConstructs.NValue, A]
  val monadAggregate = smonadFromFreeS[AggregateAST, NValue]

  object Aggregate:
    given fromValue[A]: Conversion[A, Aggregate[A]] = a => compute(NValue(a))
    given fromNValue[A]: Conversion[NValue[A], Aggregate[A]] = compute(_)

    def sensor[A](a: =>A): Aggregate[A] =
      FreeS.liftM(AggregateAST.Val(() => NValue(a)))
    def compute[A](a: NValue[A]): Aggregate[A] =
      FreeS.Pure(a)
    def call[A](f: Aggregate[() => Aggregate[A]]): Aggregate[A] =
      f.flatMap(v => FreeS.liftM(AggregateAST.Call(v)))
    def exchange[A](a: Aggregate[A])(f: NValue[A] => (Aggregate[A], Aggregate[A])): Aggregate[A] =
      a.flatMap(v => FreeS.liftM(AggregateAST.Xc(v, f)))
