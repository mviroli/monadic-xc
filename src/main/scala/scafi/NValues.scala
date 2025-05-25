package scafi

object NValues:

  import Devices.*
  export NValue.{given, *}
  import FreeSMonads.*
  import FreeS.*
  import MapWithDefault.*

  enum NValueAST[A]:
    case Concrete(nvc: MapWithDefault[Device, A])
    case Self(nv: NValue[A])
    case Builtin(a: NValue[A], f: Device => Set[Device] => NValue[A] => NValue[A])
  import NValueAST.*

  type NValue[A] = Free[NValueAST, A]

  object NValue:
    import NValueInternal.*
    given toNValue[A]: Conversion[A, NValue[A]] = apply(_)
    def apply[A](a: A): NValue[A] = FreeS.liftM(Concrete(MapWithDefault(a, Map.empty)))
    def nself[A](a: NValue[A]): NValue[A] = FreeS.liftM(Self(a))
    def nfold[A](init: A)(op: (A, A) => A)(a: NValue[A]): NValue[A] =
      FreeS.liftM(
        Builtin(a, d => domain => nv => (domain - d).map(nv.concrete(using d)(using domain).get).foldLeft(init)(op).nv))
    extension [A](nv: NValue[A]) def selfValue: A = nself(nv).concrete(using selfDevice)(using Set()).a

  object nvalueAggregate extends Monad[NValue]:
    def pure[A](a: A): NValue[A] = FreeS.pure[NValueAST, [X] =>> X, A](a)
    def flatMap[A, B](ma: NValue[A])(f: A =>  NValue[B]):  NValue[B] = ma.flatMap(f)

  private[scafi] object NValueInternal:
    extension [A](a: A) def nv: NValue[A] = NValue(a)
    def fromConcrete[A](nvc: MapWithDefault[Device, A]): NValue[A] = FreeS.liftM(Concrete(nvc))
    def apply[A](a: A, map: Map[Device, A]): NValue[A] = FreeS.liftM(Concrete(MapWithDefault(a, map)))
    def localValue[A](nv: NValue[A])(using Device)(using Set[Device]): A = nv.concrete.get(summon[Device])
    extension [A](nv: NValue[A])
      def defaultValue(using Device)(using Set[Device]): A = nv.concrete.a
      def concrete(using d: Device)(using sd: Set[Device]): MapWithDefault[Device, A] = nv match
        case Pure(a) => MapWithDefault(a)
        case Suspend(Concrete(nvc)) => nvc
        case Suspend(Self(nv)) => MapWithDefault(nv.concrete.get(summon[Device]))
        case Suspend(Builtin(nv, f)) => f(d)(sd)(nv).concrete
        case FlatMap(nv, f) => nv.concrete.flatMap(x => f(x).concrete)