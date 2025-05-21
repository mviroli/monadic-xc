package scafi

object Semantics:
  import Aggregates.*
  import AggregateAST.*
  import CFreeMonads.*
  import FreeMonads.*

  export Rounds.*

  given CMonad[Round, NValue] with
    def pure[A](a: NValue[A]): Round[A] = _ => TVal(a)

    def flatMap[A, B](ma: Round[A])(f: NValue[A] => Round[B]): Round[B] = env =>
      val left2 = ma(env.enter[TNext[Any]](_.left))
      val right2 = f(left2.top)(env.enter[TNext[B]](_.right))
      TNext(left2.asInstanceOf[Tree[Any]], right2)

  extension [A](ag: Aggregate[A])
    def round: Round[A] = ag.foldMap(compiler)

  private def compiler: AggregateAST ~~> Round = new(AggregateAST ~~> Round):
    override def apply[A] =
      case Val(a) => _ => TVal(a())
      //case Builtin(a, f) => env =>
      //  TBuiltin(f(summon[Device])(env.keySet)(a))
      case Call(vf) => env =>
        val nest2 = env.enter[TCall[A]](_.nest, n => local(n.fun) == local(vf))
        val body = env.localInterpreted(vf)()
        TCall(vf.asInstanceOf[NValue[() => Aggregate[Any]]], body.round(nest2.asInstanceOf[Environment[A]]))
      case Xc(a, f) => env =>
        val l = localValue(a)
        val w = NValue(l, env.enter[TXc[A]](_.send).collectValues[A] { case tree: Tree[A] => localValue(tree.top) })
        val ret2 = f(w)._1.round(env.enter[TXc[A]](_.ret))
        val send2 = f(w)._2.round(env.enter[TXc[A]](_.send))
        TXc(ret2, send2)

  given Monad[RoundNV] with
    def pure[A](a: A): RoundNV[A] = _ => NValueConcrete(a, Map.empty)
    def flatMap[A, B](ma: RoundNV[A])(f: A => RoundNV[B]): RoundNV[B] = dev ?=> env =>
      ma(using dev)(env).flatMap(a => f(a)(using dev)(env))

  import NValueAST.*

  extension [B](env: Environment[B])
    private[scafi] def localInterpreted[A](nv: NValue[A])(using Device): A =
      val rnv = nv.foldMap(compilerNV)
      val cnv = rnv(env.asInstanceOf[Environment[Any]])
      cnv.get(summon[Device])

  private def compilerNV: NValueAST ~> RoundNV = new (NValueAST ~> RoundNV):
    override def apply[A](nast: NValueAST[A]): RoundNV[A] = nast match
      case Concrete(c) => env => c
      case Self(nv) => env => NValueConcrete(env.localInterpreted(nv), Map.empty)
