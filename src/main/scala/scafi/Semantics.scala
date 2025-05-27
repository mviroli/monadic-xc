package scafi

import scala.reflect.ClassTag

object Semantics:
  import Aggregates.*
  import Devices.*
  import NValues.{*, given}
  import AggregateAST.*
  import SMonads.*
  import FreeSMonads.*
  import NValues.NValueInternal.*

  export Rounds.*

  given SMonad[Round, NValue] with
    def pure[A](a: NValue[A]): Round[A] = env ?=> d ?=> TVal(a.concrete(using env.asInstanceOf[Environment[Any]])(using d))

    extension [A](ma: Round[A]) 
      def flatMap[B](f: NValue[A] => Round[B]): Round[B] =
        val left = ma(using Env.enter[TNext[Any]](_.left))
        val right = f(fromConcrete(left.top))(using Env.enter[TNext[B]](_.right))
        TNext(left.asInstanceOf[Tree[Any]], right)

  extension [A](ag: Aggregate[A])
    def round: Round[A] = ag.foldMap[Round](compiler)

  private[scafi] def compiler: AggregateAST ~~> Round = new(AggregateAST ~~> Round):
    override def apply[A] =
      case Val(a) => TVal(a().concrete)
      case Call(vf) =>
        val nest = Env.enter[TCall[A]](_.nest, n => localValue(fromConcrete(n.fun)) == localValue(vf))
        val body = Env.localInterpreted(vf)(using Env.as[Any])
        TCall(vf.concrete.asInstanceOf[NbrMap[() => Aggregate[Any]]], body().round(using nest.as[A]))
      case Xc(a, f) =>
        val w = NValueInternal(localValue(a), Env.enter[TXc[A]](_.send).collectValues[A]:
          case tree: Tree[A] => localValue(fromConcrete(tree.top)))
        TXc(f(w)._1.round(using Env.enter[TXc[A]](_.ret)), f(w)._2.round(using Env.enter[TXc[A]](_.send)))

  given Monad[RoundNV] with
    def pure[A](a: A): RoundNV[A] = NbrMap(a)
    extension [A](ma: RoundNV[A]) def flatMap[B](f: A => RoundNV[B]): RoundNV[B] =
      ma(using summon[Environment[Any]]).flatMap(a => f(a)) // Infinite call?

  import NValueAST.*

  extension [B](env: Environment[B])
    private[scafi] def localInterpreted[A](nv: NValue[A]): Contextual[Any, A] =
      nv.foldMap[RoundNV](compilerNV).get(summon[Device])

  private def compilerNV: NValueAST ~~> RoundNV = new(NValueAST ~~> RoundNV):
    override def apply[A]: NValueAST[A] => RoundNV[A] =
      case Concrete(c) => c
      case Self(nv) => NbrMap(summon[Environment[Any]].localInterpreted(nv))