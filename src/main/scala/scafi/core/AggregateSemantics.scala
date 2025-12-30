package scafi.core

import fplib.FreeSMonads.*

/**
 * Semantics for Aggregates, using the FreeMonad approach
 * => it stays in 20 lines!
 */

import AggregateConstructs.*
import Environments.*

trait AggregateSemanticsAPI:
  extension [A](ag: Aggregate[A]) def round: Round[A]
  given smonadRound: SMonad[Round, NValue]

object AggregateSemantics extends AggregateSemanticsAPI:

  import AggregateAST.*
  import NValueSemantics.*
  export Environments.*

  extension [A](ag: Aggregate[A])
    def round: Round[A] = ag.foldMap[Round](compiler)

  given smonadRound: SMonad[Round, NValue] with
    def pure[A](a: NValue[A]): Round[A] = TVal(a.asNbrMap)

    extension [A](ma: Round[A]) 
      def flatMap[B](f: NValue[A] => Round[B]): Round[B] =
        val left = ma(using Env.enter[TNext[_]](_.left))
        val right = f(NValue(left.top))(using Env.enter[TNext[_]](_.right))
        TNext(left, right)

  private[scafi] def compiler: AggregateAST ~~> Round = new(AggregateAST ~~> Round):
    override def apply[A] =
      case Val(nv) =>
        TVal(nv().asNbrMap)
      case Call(nvFun) =>
        val nestEnv = Env.enter[TCall[_]](_.nest, n => n.fun.get(summon[Device]) == nvFun.localValue)
        TCall(nvFun.asNbrMap, (nvFun.localValue)().round(using nestEnv))
      case Xc(nvInit, nvFun) =>
        val w = NValue(NbrMap(nvInit.localValue, Env.enter[TXc[A]](_.send).collect:
          case device -> (tree: Tree[A]) => device -> tree.top.get(summon[Device])))
        TXc(nvFun(w)._1.round(using Env.enter[TXc[_]](_.ret)), nvFun(w)._2.round(using Env.enter[TXc[_]](_.send)))