package scafi.core

import scala.reflect.ClassTag

object AggregateSemantics:
  import Aggregates.*
  import AggregateAST.*
  import Devices.*
  import NValueSemantics.*
  import NValues.NValueInternal.*
  import NValues.given
  import smonads.FreeSMonads.*
  import smonads.SMonads.*

  export Environments.*

  extension [A](ag: Aggregate[A])
    def round: Round[A] = ag.foldMap[Round](compiler)

  given SMonad[Round, NValue] with
    def pure[A](a: NValue[A]): Round[A] = TVal(a.asNbrMap(using Env.as[Any]))

    extension [A](ma: Round[A]) 
      def flatMap[B](f: NValue[A] => Round[B]): Round[B] =
        val left = ma(using Env.enter[TNext[Any]](_.left))
        val right = f(fromNbrMap(left.top))(using Env.enter[TNext[B]](_.right))
        TNext(left.asInstanceOf[Tree[Any]], right)

  private[scafi] def compiler: AggregateAST ~~> Round = new(AggregateAST ~~> Round):
    override def apply[A] =
      case Val(nv) => TVal(nv().asNbrMap)
      case Call(nvFun) =>
        val nestEnv = Env.enter[TCall[A]](_.nest, n => localValue(fromNbrMap(n.fun)) == localValue(nvFun))
        val bodyAgg = Env.localInterpreted(nvFun)(using Env.as[Any])
        TCall(nvFun.asNbrMap.asInstanceOf[NbrMap[() => Aggregate[Any]]], bodyAgg().round(using nestEnv.as[A]))
      case Xc(nvInit, nvFun) =>
        val w = NValueInternal(localValue(nvInit), Env.enter[TXc[A]](_.send).collectValues[A]:
          case tree: Tree[A] => localValue(fromNbrMap(tree.top)))
        TXc(nvFun(w)._1.round(using Env.enter[TXc[A]](_.ret)), nvFun(w)._2.round(using Env.enter[TXc[A]](_.send)))