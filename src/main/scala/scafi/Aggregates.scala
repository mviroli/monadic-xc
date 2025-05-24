package scafi

import scala.reflect.ClassTag

object Aggregates:
  import FreeSMonads.*
  export NValues.{*, given}
  export Devices.*
  export Aggregate.{*, given}

  enum AggregateAST[A]:
    case Val(a: () => NValue[A])
    case Call(f: NValue[() => Aggregate[A]])
    case Xc(a: NValue[A], f: NValue[A] => (Aggregate[A], Aggregate[A]))

  type Aggregate[A] = FreeS[AggregateAST, NValue, A]
  object monadAggregate extends SMonad[Aggregate, NValue]:
    def pure[A](a: NValue[A]): Aggregate[A] = FreeS.pure[AggregateAST, NValue, A](a)
    def flatMap[A, B](ma: Aggregate[A])(f: NValue[A] => Aggregate[B]): Aggregate[B] = ma.flatMap(f)


  object Aggregate:
    given fromValue[A]: Conversion[A, Aggregate[A]] = NValue.apply(_)
    given fromNValue[A]: Conversion[NValue[A], Aggregate[A]] = compute(_)

    def sensor[A](a: =>A): Aggregate[A] = FreeS.liftM(AggregateAST.Val(() => NValue(a)))
    def compute[A](a: NValue[A]): Aggregate[A] = FreeS.Pure(a)
    def call[A](f: Aggregate[() => Aggregate[A]]): Aggregate[A] = f.flatMap(v => FreeS.liftM(AggregateAST.Call(v)))
    def exchange[A](a: Aggregate[A])(f: NValue[A] => (Aggregate[A], Aggregate[A])): Aggregate[A] = a.flatMap(v => FreeS.liftM(AggregateAST.Xc(v, f)))
  export Semantics.*

  @main def tryWit =
    val ag = sensor(10)
    //println(summon[SMonad[Aggregate,NValue]])
