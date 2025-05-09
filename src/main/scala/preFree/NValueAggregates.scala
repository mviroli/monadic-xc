package preFree

import scafi.Devices.{Domain, selfDevice}
import FreeMonads.{Free, Monad, ~>}
import scafi.NValues.NValue
import scafi.{Devices, NValues}

object NValueAggregates:
  import FreeMonads.*
  export Devices.{*, given}
  export NValues.{*, given}
  export Executor.*
  export Executor.Tree.*
  export Aggregate.{*, given}

  enum AggregateDSL[A]:
    case Rep(a: A, f: A => Aggregate[A])
    case Call(f: Aggregate[() => Aggregate[A]])

  type Aggregate[A] = Free[AggregateDSL, A]

  object Aggregate:
    import AggregateDSL.*
    given [A <: NValue[Any]]: Conversion[A, Aggregate[A]] = compute
    given [A]: Conversion[() => Aggregate[A], Aggregate[() => Aggregate[A]]] = Free.pure
    def compute[A <: NValue[Any]](a: A): Aggregate[A] = Free.pure(a)
    //def rep[A <: NValue[Any]](a: A)(f: A => Aggregate[A]): Aggregate[A] = Free.liftM(Rep(a, f))
    def rep[A](a: NValue[A])(f: NValue[A] => Aggregate[NValue[A]]): Aggregate[NValue[A]] = Free.liftM(Rep(a, f))
    //def call[A <: NValue[Any]](f: Aggregate[() => Aggregate[A]]): Aggregate[A] = Free.liftM(Call(f))
    def call[A](f: Aggregate[() => Aggregate[NValue[A]]]): Aggregate[NValue[A]] = Free.liftM(Call(f))

    def rand: Aggregate[NValue[Int]] = rep(0)(n => n)

  object Executor:
    import NValueAggregates.*

    enum Tree[A]:
      case TRep(res: A, nest: Tree[A])
      case TVal(res: A)
      case TNext(left: Tree[Any], right: Tree[A]) extends Tree[A]
      case TCall(fun: Tree[() => Aggregate[A]], nest: Tree[A])
      case TEmpty()

      def top: A = this match
        case TRep(a, _) => a
        case TVal(a) => a
        case TNext(_, r) => r.top
        case TCall(_, n) => n.top

      def cleanEmpty(t: Tree[A])(using Domain): Tree[A] =
        if this == TEmpty() || !summon[Domain].contains(selfDevice) then t else this

    import AggregateDSL.*
    import Tree.*
    def let[A, B](a: A)(f: A => B): B = f(a)

    type AggregateSemantics[A] = Domain ?=> Tree[A] => Tree[A]

    given Monad[AggregateSemantics] with
      def pure[A](a: A): AggregateSemantics[A] = _ => TVal(a)
      def flatMap[A, B](ma: AggregateSemantics[A])(f: A => AggregateSemantics[B]): AggregateSemantics[B] =
        t => t.cleanEmpty(TNext(TEmpty(), TEmpty())) match
          case TNext(left: Tree[A], right) => let(ma(left))(left2 => TNext(left2, f(left2.top)(right)))

    def compiler: AggregateDSL ~> AggregateSemantics = new (AggregateDSL ~> AggregateSemantics):
      override def apply[A](fa: AggregateDSL[A]): AggregateSemantics[A] = fa match
        case Rep(a, f) => t => t.cleanEmpty(TEmpty()) match
          case TRep(x: A, nest: Tree[A]) =>
            let(f(x).foldMap(compiler).apply(nest))(nest2 => TRep(nest2.top, nest2))
          case _ =>
            TRep(a, TEmpty())
        case Call(f) => t => t.cleanEmpty(TCall(TEmpty(), TEmpty())) match
          case TCall(fun, nest) => let(f.foldMap(compiler).apply(fun)): fun2 =>
            if fun != TEmpty() && fun.top == fun2.top
            then TCall(fun2, fun2.top().foldMap(compiler).apply(nest))
            else TCall(fun2, fun2.top().foldMap(compiler).apply(using summon[Domain] - selfDevice)(TEmpty()))


