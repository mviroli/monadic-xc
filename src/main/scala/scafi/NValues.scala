package scafi

object NValues:

  import Devices.*
  export NValue.{given, *}
  import FreeSMonads.*
  import FreeS.*

  case class NValueConcrete[+A] private[scafi] (a: A, map: Map[Device, A]):
    override def toString: String = a.toString + "[" + map.mkString(", ") + "]"
    def flatMap[B](f: A => NValueConcrete[B]): NValueConcrete[B] =
      NValueConcrete(f(a).a, (map.keySet ++ f(a).map.keySet).map(k => (k, f(this.get(k)).get(k))).toMap)
    def map[B](f: A => B): NValueConcrete[B] = NValueConcrete(f(this.a), map.map((k, v) => (k, f(v))))
    def get(d: Device): A = map.getOrElse(d, a)
    def asValue: A = if map.isEmpty || map.values.forall(v => v == a) then a else throw new MatchError(this)

  object NValueConcrete:
    def apply[A](a: A): NValueConcrete[A] = new NValueConcrete(a, Map.empty)
    def apply[A](a: A, map: Map[Device, A]): NValueConcrete[A] = new NValueConcrete(a, map.filterNot((_, v) => v == a))
    extension [A](a: A) def |>(e: (Device, A)*): NValueConcrete[A] = apply(a, e.toMap)

  enum NValueAST[A]:
    case Concrete(nvc: NValueConcrete[A])
    case Self(nv: NValue[A])
    case Builtin(a: NValue[A], f: Device => Set[Device] => NValue[A] => NValue[A])
  import NValueAST.*

  type NValue[A] = Free[NValueAST, A]

  object NValue:
    import NValueInternal.*
    given g[A]: Conversion[A, NValue[A]] = apply(_)
    def apply[A](a: A): NValue[A] = FreeS.liftM(Concrete(NValueConcrete(a, Map.empty)))
    def nself[A](a: NValue[A]): NValue[A] = FreeS.liftM(Self(a))
    def nfold[A](init: A)(op: (A, A) => A)(a: NValue[A]): NValue[A] =
      FreeS.liftM(
        Builtin(a, d => domain => nv => (domain - d).map(nv.concrete(using d)(using domain).get).foldLeft(init)(op).nv))
    extension [A](nv: NValue[A]) def selfValue(a: A): Boolean = nself(nv).concrete(using selfDevice)(using Set()).a == a

  private[scafi] object NValueInternal:
    extension [A](a: A) def nv: NValue[A] = NValue(a)
    def fromConcrete[A](nvc: NValueConcrete[A]): NValue[A] = FreeS.liftM(Concrete(nvc))
    def apply[A](a: A, map: Map[Device, A]): NValue[A] = FreeS.liftM(Concrete(NValueConcrete(a, map)))
    def localValue[A](nv: NValue[A])(using Device)(using Set[Device]): A = nv.concrete.get(summon[Device])
    extension [A](nv: NValue[A])
      def defaultValue(using Device)(using Set[Device]): A = nv.concrete.a
      def concrete(using d: Device)(using sd: Set[Device]): NValueConcrete[A] = nv match
        case Pure(a) => NValueConcrete(a)
        case Suspend(Concrete(nvc)) => nvc
        case Suspend(Self(nv)) => NValueConcrete(nv.concrete.get(summon[Device]))
        case Suspend(Builtin(nv, f)) => f(d)(sd)(nv).concrete
        case FlatMap(nv, f) => nv.concrete.flatMap(x => f(x).concrete)