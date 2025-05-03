package scafi

import preFree.NValueAggregates.AggregateDSL
import preFree.NValueAggregates.Executor.AggregateSemantics

object CFreeMonads:
  trait CMonad[M[_], C[_]]:
    def pure[A](a: C[A]): M[A]
    def flatMap[A, B](ma: M[A])(f: C[A] => M[B]): M[B]

  object CMonad:
    def apply[M[_], C[_]](using cmonad: CMonad[M,C]): CMonad[M,C] = cmonad

  trait ~~>[F[_], G[_]]:
    def apply[A]: F[A] => G[A]

  trait CFree[M[_], C[_], A]:

    import CFree.*

    def flatMap[B](f: C[A] => CFree[M, C, B]): CFree[M, C, B] = FlatMap(this, f)

    def map[B](f: C[A] => C[B]): CFree[M, C, B] = flatMap(a => pure(f(a)))

    def foldMap[G[_]](natTrans: M ~~> G)(using CMonad[G, C]): G[A] = this match
      case Pure(a) => CMonad[G, C].pure(a)
      case Suspend(ma) => natTrans.apply(ma)
      case FlatMap(fa, f) => // need a G[B]
        CMonad[G, C].flatMap(fa.foldMap(natTrans))(a => f(a).foldMap(natTrans))

  object CFree:
    def pure[M[_], C[_], A](a: C[A]): CFree[M, C, A] = Pure(a)
    def liftM[M[_], C[_], A](ma: M[A]): CFree[M, C, A] = Suspend(ma)
    case class Pure[M[_], C[_], A](a: C[A]) extends CFree[M, C, A]
    case class FlatMap[M[_], C[_], A, B](fa: CFree[M, C, A], f: C[A] => CFree[M, C, B]) extends CFree[M, C, B]
    case class Suspend[M[_], C[_], A](ma: M[A]) extends CFree[M, C, A]

object AggregatesNFWithFree:
  import CFreeMonads.*
  export NValues.{*, given}
  export Aggregate.{*, given}

  enum AggregateAST[A]:
    case Val(a: NValue[A])
    case Rep(a: NValue[A], f: NValue[A] => Aggregate[A])
    case Call(f: Aggregate[() => Aggregate[A]])
  import AggregateAST.*

  type Aggregate[A] = CFree[AggregateAST, NValue, A]

  object Aggregate:
    given [A]: Conversion[A, Aggregate[A]] = compute
    given [A]: Conversion[NValue[A], Aggregate[A]] = compute

    def compute[A](a: NValue[A]): Aggregate[A] = CFree.liftM(Val(a))
    def rep[A](a: NValue[A])(f: NValue[A] => Aggregate[A]): Aggregate[A] = CFree.liftM(Rep(a, f))
    def call[A](f: Aggregate[() => Aggregate[A]]): Aggregate[A] = CFree.liftM(Call(f))

  export Devices.*
  enum Tree[A]:
    case TRep(res: NValue[A], nest: Tree[A])
    case TVal(res: NValue[A])
    case TNext(left: Tree[Any], right: Tree[A]) extends Tree[A]
    case TCall(fun: Tree[() => Aggregate[A]], nest: Tree[A])
    case TEmpty()

    def top: NValue[A] = this match
      case TRep(a, _) => a
      case TVal(a) => a
      case TNext(_, r) => r.top
      case TCall(_, n) => n.top

  import Tree.*
  object Matchers:
    def cleanEmpty[A](t: Tree[A])(orElse: Tree[A])(using Domain): Tree[A] =
      if t == TEmpty() || !summon[Domain].contains(selfDevice) then orElse else t
    object Next:
      def unapply[A](t: Tree[A]): TNext[A] =
        if t == TEmpty() then TNext(TEmpty(), TEmpty()) else t.asInstanceOf[TNext[A]]
    object Rep:
      def unapply[A](t: Tree[A]): TRep[A] = t.asInstanceOf[TRep[A]]

  def let[A, B](a: A)(f: A => B): B = f(a)

  import Tree.*
  type Context[A] = Map[Device, Tree[A]]
  type Round[A] = Context[A] => Tree[A]

  def local[A](t: Tree[A]): Context[A] = Map(selfDevice -> t)
  def restrict[A](c: Context[A])(domain: Domain): Context[A] = c.filter((d, _) => domain.contains(d))

  extension [K, V, W](m: Map[K, V]) def collectValues(pf: PartialFunction[V, W]): Map[K, W] =
    m.collect:
      case device -> v if pf.isDefinedAt(v) => device -> pf(v)

  given CMonad[Round, NValue] with
    def pure[A](a: NValue[A]): Round[A] = _ => TVal(a)
    def flatMap[A, B](ma: Round[A])(f: NValue[A] => Round[B]): Round[B] = map =>
      val cleanedMap = map.collectValues(t => Matchers.cleanEmpty(t)(TNext(TEmpty(), TEmpty())))
      val left2 = ma(cleanedMap.collectValues{ case Matchers.Next(left: Tree[A], right: Tree[B]) => left})
      val right2 = f(left2.top)(cleanedMap.collectValues{ case Matchers.Next(left: Tree[A], right: Tree[B]) => right})
      TNext(left2.asInstanceOf[Tree[Any]], right2)

  def compiler: AggregateAST ~~> Round = new (AggregateAST ~~> Round):
    override def apply[A] =
      case Val(a) => _ => TVal(a)
      case Rep(a, f) => map => map.applyOrElse(selfDevice, _ => TEmpty[A]()) match
        case TRep(x, nest) =>
          val nest2 = f(x).foldMap(compiler).apply(local(nest))
          TRep(nest2.top, nest2)
        case _ => TRep(a, TEmpty())
      case Call(f) => map =>
        val fun2 = f.foldMap(compiler).apply(map.collectValues { case TCall(fun, _) => fun })
        val preNest2 = if fun2 == TEmpty()
          then local(TEmpty())
          else map.collectValues:
            case TCall(fun, nest) if fun.top(selfDevice) == fun2.top(selfDevice) => nest
        val nest2 = if preNest2.isDefinedAt(selfDevice) then preNest2 else preNest2 + (selfDevice -> TEmpty())
        TCall(fun2, fun2.top.apply(selfDevice)().foldMap(compiler).apply(nest2))

@main def testNFwF =
  import AggregatesNFWithFree.*
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
  println(ag3.foldMap(compiler).apply(local(TEmpty())))

@main def playNFwF =
  import AggregatesNFWithFree.*
  def counter = rep(0)(for i <- _ yield i + 1)
  def ag = for
    a1 <- rep(0)(for i <- _ yield i + 1)
    a2 <- rep(0)(for i <- _ yield i + 1)
  yield for
    i1 <- a1
    i2 <- a2
  yield i1 + i2
  def caller = call(() => counter)