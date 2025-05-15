package scafi

object Semantics:
  import Aggregates.*
  import AggregateAST.*
  import CFreeMonads.*

  export Rounds.*

  given CMonad[Round, NValue] with
    def pure[A](a: NValue[A]): Round[A] = _ => TVal(a)

    def flatMap[A, B](ma: Round[A])(f: NValue[A] => Round[B]): Round[B] = ctx =>
      val left2 = ma(ctx.enter[TNext[Any]](_.left))
      val right2 = f(left2.top)(ctx.enter[TNext[B]](_.right))
      TNext(left2, right2)

  extension [A](ag: Aggregate[A])
    def round: Round[A] = ag.foldMap(compiler)

  extension [A](a: A) def nv: NValue[A] = NValue(a, Map.empty)

  private def compiler: AggregateAST ~~> Round = new(AggregateAST ~~> Round):
    override def apply[A] =
      case Val(a) => _ => TVal(a())
      case Call(f) => ctx =>
        val fun2 = f.round(ctx.enter[TCall[A]](_.fun))
        val nest2 = ctx.enter[TCall[A]](_.nest, n => local(n.fun.top) == local(fun2.top))
        TCall(fun2.asInstanceOf[Tree[() => Aggregate[Any]]], fun2.top.get(summon[Device])().round(nest2))
      case Xc(a, f) => ctx =>
        val init2 = a.round(ctx.enter[TXc[A]](_.init))
        val l = local(init2.top)
        val w = NValue(l, ctx.enter[TXc[A]](_.send).collectValues[A] { case tree: Tree[A] => local(tree.top) })
        val ret2 = f(w)._1.round(ctx.enter[TXc[A]](_.ret))
        val send2 = f(w)._2.round(ctx.enter[TXc[A]](_.send))
        TXc(init2, ret2, send2)

