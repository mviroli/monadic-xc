package scafi

object Semantics:
  import Aggregates.*
  import AggregateAST.*
  import CFreeMonads.*

  export Rounds.*

  given CMonad[Round, NValue] with
    def pure[A](a: NValue[A]): Round[A] = _ => TVal(a)

    def flatMap[A, B](ma: Round[A])(f: NValue[A] => Round[B]): Round[B] = env =>
      val left2 = ma(env.enter[TNext[Any]](_.left))
      val right2 = f(left2.top)(env.enter[TNext[B]](_.right))
      TNext(left2, right2)

  extension [A](ag: Aggregate[A])
    def round: Round[A] = ag.foldMap(compiler)

  private def compiler: AggregateAST ~~> Round = new(AggregateAST ~~> Round):
    override def apply[A] =
      case Val(a) => _ => TVal(a())
      case Builtin(a, f) => env =>
        val nest = a.round(env.enter[TBuiltin[A]](_.nest))
        TBuiltin(f(summon[Device])(env.keySet)(nest.top), nest)
      case Call(vf) => env =>
        val nest2 = env.enter[TCall[A]](_.nest, n => local(n.fun) == local(vf))
        TCall(vf.asInstanceOf[NValue[() => Aggregate[Any]]], vf.get(summon[Device])().round(nest2))
      case Xc(a, f) => env =>
        //val init2 = a.round(env.enter[TXc[A]](_.init))
        val l = local(a)
        val w = NValue(l, env.enter[TXc[A]](_.send).collectValues[A] { case tree: Tree[A] => local(tree.top) })
        val ret2 = f(w)._1.round(env.enter[TXc[A]](_.ret))
        val send2 = f(w)._2.round(env.enter[TXc[A]](_.send))
        TXc(ret2, send2)

