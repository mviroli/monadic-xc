package scafi.core

/**
 * Semantics for Aggregates, using the FreeMonad approach
 * => it stays in 20 lines!
 */

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
    def pure[A](a: NValue[A]): Round[A] = TVal(a.asNbrMap)

    extension [A](ma: Round[A]) 
      def flatMap[B](f: NValue[A] => Round[B]): Round[B] =
        val left = ma(using Env.enter[TNext[_]](_.left))
        val right = f(fromNbrMap(left.top))(using Env.enter[TNext[_]](_.right))
        TNext(left, right)

  private[scafi] def compiler: AggregateAST ~~> Round = new(AggregateAST ~~> Round):
    override def apply[A] =
      case Val(nv) => TVal(nv().asNbrMap)
      case Call(nvFun) =>
        val nestEnv = Env.enter[TCall[_]](_.nest, n => localValue(fromNbrMap(n.fun)) == localValue(nvFun))
        TCall(nvFun.asNbrMap, Env.localInterpreted(nvFun)().round(using nestEnv))
      case Xc(nvInit, nvFun) =>
        val w = NValueInternal(localValue(nvInit), Env.enter[TXc[A]](_.send).collectValues:
          case tree: Tree[A] => localValue(fromNbrMap(tree.top)))
        TXc(nvFun(w)._1.round(using Env.enter[TXc[_]](_.ret)), nvFun(w)._2.round(using Env.enter[TXc[_]](_.send)))