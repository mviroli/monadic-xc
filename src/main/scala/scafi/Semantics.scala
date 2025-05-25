package scafi

object Semantics:
  import Aggregates.*
  import NValues.{*, given}
  import AggregateAST.*
  import SMonads.*
  import FreeSMonads.*
  import NValues.NValueInternal.*

  export Rounds.*

  given SMonad[Round, NValue] with
    def pure[A](a: NValue[A]): Round[A] = env => TVal(a.concrete(using summon[Device])(using env.keySet))

    def flatMap[A, B](ma: Round[A])(f: NValue[A] => Round[B]): Round[B] = env =>
      val left = ma(env.enter[TNext[Any]](_.left))
      val right = f(fromConcrete(left.top))(env.enter[TNext[B]](_.right))
      TNext(left.asInstanceOf[Tree[Any]], right)

  extension [A](ag: Aggregate[A])
    def round: Round[A] = ag.foldMap(compiler)

  private[scafi] def compiler: AggregateAST ~~> Round = new(AggregateAST ~~> Round):
    override def apply[A] =
      case Val(a) => env => TVal(a().concrete(using summon[Device])(using env.keySet))
      case Call(vf) => env =>
        val nest = env.enter[TCall[A]](_.nest, n => localValue(fromConcrete(n.fun))(using summon[Device])(using env.keySet) == localValue(vf)(using summon[Device])(using env.keySet))
        val body = env.localInterpreted(vf)()
        TCall(vf.concrete(using summon[Device])(using env.keySet).asInstanceOf[MapWithDefault[Device, () => Aggregate[Any]]], body.round(nest.asInstanceOf[Environment[A]]))
      case Xc(a, f) => env =>
        val l = localValue(a)(using summon[Device])(using env.keySet)
        val w = NValueInternal(l, env.enter[TXc[A]](_.send).collectValues[A]:
          case tree: Tree[A] => NValueInternal.localValue(fromConcrete(tree.top))(using summon[Device])(using env.keySet))
        TXc(f(w)._1.round(env.enter[TXc[A]](_.ret)), f(w)._2.round(env.enter[TXc[A]](_.send)))

  given Monad[RoundNV] with
    def pure[A](a: A): RoundNV[A] = _ => MapWithDefault(a, Map.empty)
    def flatMap[A, B](ma: RoundNV[A])(f: A => RoundNV[B]): RoundNV[B] = dev ?=> env =>
      ma(using dev)(env).flatMap(a => f(a)(using dev)(env))

  import NValueAST.*

  extension [B](env: Environment[B])
    private[scafi] def localInterpreted[A](nv: NValue[A])(using Device): A =
      val rnv = nv.foldMap(compilerNV)
      val cnv = rnv(env.asInstanceOf[Environment[Any]])
      cnv.get(summon[Device])

  private def compilerNV: NValueAST ~~> RoundNV = new (NValueAST ~~> RoundNV):
    override def apply[A]: NValueAST[A] => RoundNV[A] = 
      case Concrete(c) => env => c
      case Self(nv) => env => MapWithDefault(env.localInterpreted(nv), Map.empty)
