package scafi

import scala.reflect.ClassTag

object Aggregates:
  import CFreeMonads.*
  export NValues.{*, given}
  export Aggregate.{*, given}

  enum AggregateAST[A]:
    case Val(a: NValue[A])
    case Call(f: Aggregate[() => Aggregate[A]])
    case Xc(a: Aggregate[A], f: NValue[A] => (Aggregate[A], Aggregate[A]))

  import AggregateAST.*

  type Aggregate[A] = CFree[AggregateAST, NValue, A]

  object Aggregate:
    given [A]: Conversion[A, Aggregate[A]] = compute
    given [A]: Conversion[NValue[A], Aggregate[A]] = compute

    def compute[A](a: NValue[A]): Aggregate[A] = CFree.liftM(Val(a))
    def call[A](f: Aggregate[() => Aggregate[A]]): Aggregate[A] = CFree.liftM(Call(f))
    def exchange[A](a: Aggregate[A])(f: NValue[A] => (Aggregate[A], Aggregate[A])): Aggregate[A] = CFree.liftM(Xc(a, f))
    def rep[A](a: NValue[A])(f: NValue[A] => Aggregate[A]): Aggregate[A] = exchange(compute(a))(x => {val r = f(NValue(x(selfDevice))); (r, r)})

  export Devices.*
  enum Tree[A]:
    case TRep(res: NValue[A], nest: Tree[A])
    case TVal(res: NValue[A])
    case TNext(left: Tree[Any], right: Tree[A]) extends Tree[A]
    case TCall(fun: Tree[() => Aggregate[A]], nest: Tree[A])
    case TXc(init: Tree[A], ret: Tree[A], send: Tree[A])
    case TEmpty()

    def top: NValue[A] = this match
      case TRep(a, _) => a
      case TVal(a) => a
      case TNext(_, r) => r.top
      case TCall(_, n) => n.top
      case TXc(_, ret, _) => ret.top

  import Tree.*

  object Contexts:
    type Context[A] = Map[Device, Tree[A]]
    def local[A](using Device)(t: Tree[A]): Context[A] = Map(summon[Device] -> t)
    def restrict[A](c: Context[A])(domain: Domain): Context[A] = c.filter((d, _) => domain.contains(d))

    extension [A, W](c: Context[A])
      def enter[B: ClassTag](f: B => Any, p: B => Boolean = (b: B) => true): Context[W] =
        c.collectValues:
          case v: B if p(v) => f(v).asInstanceOf[Tree[W]]

  export Contexts.*
  type Round[A] = Device ?=> Context[A] => Tree[A]

  extension [K, V](c: Map[K, V])
    def collectValues[W](pf: PartialFunction[V, W]): Map[K, W] =
      c.collect:
        case device -> v if pf.isDefinedAt(v) => device -> pf(v)


  given CMonad[Round, NValue] with
    def pure[A](a: NValue[A]): Round[A] = _ => TVal(a)
    def flatMap[A, B](ma: Round[A])(f: NValue[A] => Round[B]): Round[B] = ctx =>
      val left2 = ma(ctx.enter[TNext[Any]](_.left))
      val right2 = f(left2.top)(ctx.enter[TNext[B]](_.right))
      TNext(left2.asInstanceOf[Tree[Any]], right2)

  extension [A](ag: Aggregate[A])
    def round: Round[A] = ag.foldMap(compiler)

  private def compiler: AggregateAST ~~> Round = new (AggregateAST ~~> Round):
    override def apply[A] =
      case Val(a) => _ => TVal(a)
      case Call(f) => ctx =>
        val fun2 = f.round(ctx.enter[TCall[A]](_.fun))
        val nest2 = ctx.enter[TCall[A]](_.nest, _.fun.top(summon[Device]) == fun2.top(summon[Device])).asInstanceOf[Context[A]]
        TCall(fun2, fun2.top.apply(summon[Device])().round(nest2))
      case Xc(a, f) => ctx =>
        val init2 = a.round(ctx.enter[TXc[A]](_.init))
        val l = init2.top.apply(summon[Device])
        val w = NValue(l, ctx.enter[TXc[A]](_.send).collectValues[A]((tree: Tree[A]) => tree.top(summon[Device])))
        val ret2 = f(w)._1.round(ctx.enter[TXc[A]](_.ret))
        val send2 = f(w)._2.round(ctx.enter[TXc[A]](_.send))
        TXc(init2, ret2, send2)


@main def testNFwF =
  import Aggregates.{*, given}
  import Tree.*
  def ag1: Aggregate[Int] = 10
  def ag2: Aggregate[Int] = 20
  def ag3 = for
    a1 <- ag1
    a2 <- ag2
  yield for
    i1 <- a1
    i2 <- a2
  yield i1 + i2
  given Device = selfDevice
  println(ag3.round(local(TEmpty())))

@main def playNFwF =
  import Aggregates.{*, given}
  def counter = rep(0)(for i <- _ yield i + 1)
  def ag = for
    a1 <- rep(0)(for i <- _ yield i + 1)
    a2 <- rep(0)(for i <- _ yield i + 1)
  yield for
    i1 <- a1
    i2 <- a2
  yield i1 + i2
  def caller = call(() => counter)

  /*
  object Matchers:
    def cleanEmpty[A](t: Tree[A])(orElse: Tree[A])(using Domain): Tree[A] =
      if t == TEmpty() || !summon[Domain].contains(selfDevice) then orElse else t
    object Next:
      def unapply[A](t: Tree[A]): TNext[A] =
        if t == TEmpty() then TNext(TEmpty(), TEmpty()) else t.asInstanceOf[TNext[A]]
    object Rep:
      def unapply[A](t: Tree[A]): TRep[A] = t.asInstanceOf[TRep[A]]
    object Xc:
      def unapply[A](t: Tree[A]): TXc[A] =
        if t == TEmpty() then TXc(TEmpty(), TEmpty()) else t.asInstanceOf[TXc[A]]
  */