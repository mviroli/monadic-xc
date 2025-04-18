package scafi


trait Monad[M[_]]:
  def pure[A](a: A): M[A]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

object Monad:
  def apply[M[_]](using monad: Monad[M]): Monad[M] = monad

trait ~>[F[_], G[_]]:
  def apply[A](fa: F[A]): G[A]

trait Free[M[_], A]:

  import Free.*
  def flatMap[B](f: A => Free[M, B]): Free[M, B] = FlatMap(this, f)
  def map[B](f: A => B): Free[M, B] = flatMap(a => pure(f(a)))
  def foldMap[G[_] : Monad](natTrans: M ~> G): G[A] = this match
    case Pure(a) => Monad[G].pure(a)
    case Suspend(ma) => natTrans.apply(ma)
    case FlatMap(fa, f) => // need a G[B]
      Monad[G].flatMap(fa.foldMap(natTrans))(a => f(a).foldMap(natTrans))

object Free:
  def pure[M[_], A](a: A): Free[M, A] = Pure(a)
  def liftM[M[_], A](ma: M[A]): Free[M, A] = Suspend(ma)
  case class Pure[M[_], A](a: A) extends Free[M, A]
  case class FlatMap[M[_], A, B](fa: Free[M, A], f: A => Free[M, B]) extends Free[M, B]
  case class Suspend[M[_], A](ma: M[A]) extends Free[M, A]

object FreeAggregate:

  enum AggregateDSL[A]:
    case Rep(a: A, f: A => Aggregate[A])
    case Call(f: Aggregate[() => Aggregate[A]])

  type Aggregate[A] = Free[AggregateDSL, A]

  object Aggregate:
    import AggregateDSL.*
    given [A]: Conversion[A, Aggregate[A]] = (a: A) => compute(a)
    def compute[A](a: A): Aggregate[A] = Free.pure(a)
    def rep[A](a: A)(f: A => Aggregate[A]): Aggregate[A] = Free.liftM(Rep(a, f))
    def call[A](f: Aggregate[() => Aggregate[A]]): Aggregate[A] = Free.liftM(Call(f))

  object Executor:
    import Aggregate.{*, given}

    enum Tree[A]:
      case TRep(res: A, nest: Tree[A])
      case TVal(res: A)
      case TNext(left: Tree[Any], right: Tree[A]) extends Tree[A]
      case TCall(fun: Tree[() => Aggregate[A]], nest: Tree[A])
      case TEmpty()

      def top: A = this match
        case TRep(a, _) => a
        case TVal(a) => a
        case TNext(_, r) => r.top
        case TCall(_, n) => n.top

      def cleanEmpty(t: Tree[A]): Tree[A] = if this == TEmpty() then t else this

    import Tree.*
    opaque type Device = Int
    val selfDevice: Device = 0
    type Domain = Set[Device]

    import Free.*
    import AggregateDSL.*
    def let[A, B](a: A)(f: A => B): B = f(a)

    type AggregateSemantics[A] = Domain ?=> Tree[A] => Tree[A]

    given Monad[AggregateSemantics] with
      def pure[A](a: A): AggregateSemantics[A] = _ => TVal(a)
      def flatMap[A, B](ma: AggregateSemantics[A])(f: A => AggregateSemantics[B]): AggregateSemantics[B] =
        t => t.cleanEmpty(TNext(TEmpty(), TEmpty())) match
          case TNext(left: Tree[A], right) => let(ma(left))(left2 => TNext(left2, f(left2.top)(right)))

    def compiler: AggregateDSL ~> AggregateSemantics = new (AggregateDSL ~> AggregateSemantics):
      override def apply[A](fa: AggregateDSL[A]): AggregateSemantics[A] = fa match
        case Rep(a, f) =>
          case TRep(x: A, nest: Tree[A]) if summon[Domain].contains(selfDevice) =>
            let(f(x).foldMap(compiler).apply(nest))(nest2 => TRep(nest2.top, nest2))
          case _ =>
            TRep(a, TEmpty())
        case Call(f) => t => t.cleanEmpty(TCall(TEmpty(), TEmpty())) match
            case TCall(fun, nest) => let(f.foldMap(compiler).apply(fun)): fun2 =>
              if fun != TEmpty() && fun.top == fun2.top
              then TCall(fun2, fun2.top().foldMap(compiler).apply(nest))
              else TCall(fun2, fun2.top().foldMap(compiler).apply(using summon[Domain] - selfDevice)(TEmpty()))
    /*
    def compiler[A]: Aggregate[A] => AggregateSemantics[A] =
      case Suspend(Rep(a, f)) =>
        case TRep(x, nest) if summon[Domain].contains(selfDevice) => let(compiler(f(x))(nest)): nest2 =>
          TRep(nest2.top, nest2)
        case _ =>
          TRep(a, TEmpty())
      case Suspend(Call(f)) => t => t.cleanEmpty(TCall(TEmpty(), TEmpty())) match
        case TCall(fun, nest) => let(compiler(f)(fun)): fun2 =>
          if fun != TEmpty() && fun.top == fun2.top
            then TCall(fun2, compiler(fun2.top())(nest))
            else TCall(fun2, compiler(fun2.top())(using summon[Domain] - selfDevice)(TEmpty()))
      case Pure(a) =>
        _ => TVal(a)
      case FlatMap(aggregate, f): FlatMap[AggregateDSL, Any, A] => t => t.cleanEmpty(TNext(TEmpty(), TEmpty())) match
        case TNext(left, right) => let(compiler(aggregate)(left)): left2 =>
          TNext(left2, compiler(f(left2.top))(right))
    */

  import Aggregate.{*, given}

  def rand = rep(0)(n => for
    i <- rep(1)(identity)
  yield n + i)


  @main def play =
    import Executor.*
    import Tree.*
    given Domain = Set(selfDevice)
    //println(compiler(rand)(TRep(0,TEmpty())))