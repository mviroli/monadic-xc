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

  object Aggregate:
    given [A]: Conversion[A, Aggregate[A]] = NValue.apply(_)
    given [A]: Conversion[NValue[A], Aggregate[A]] = compute(_)

    def sensor[A](a: =>A): Aggregate[A] = FreeS.liftM(AggregateAST.Val(() => NValue(a)))
    def compute[A](a: NValue[A]): Aggregate[A] = FreeS.Pure(a)
    def call[A](f: Aggregate[() => Aggregate[A]]): Aggregate[A] = f.flatMap(v => FreeS.liftM(AggregateAST.Call(v)))
    def exchange[A](a: Aggregate[A])(f: NValue[A] => (Aggregate[A], Aggregate[A])): Aggregate[A] = a.flatMap(v => FreeS.liftM(AggregateAST.Xc(v, f)))
  export Semantics.*

@main def tryTest =
  import scafi.Aggregates.{*, given}

  extension [A](a: Aggregate[A])
    def evalOne(using device: Device)(initial: Environment[A] = localEnv(TEmpty[A]()), dom: Domain = Set(device)): Tree[A] =
      a.round(initial)

  val ag: Aggregate[Int] = 5
  println(ag.evalOne(using selfDevice)().top.asValue)