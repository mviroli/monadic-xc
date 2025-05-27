package scafi

object NValues:

  import Devices.*
  export NValue.{given, *}
  import FreeSMonads.*
  import FreeS.*
  import Rounds.*

  enum NValueAST[A]:
    case Concrete(nvc: NbrMap[A])
    case Self(nv: NValue[A])
    case Builtin(a: NValue[A], f: Device => Set[Device] => NValue[A] => NValue[A])
  import NValueAST.*

  type NValue[A] = Free[NValueAST, A]
  val nvalueAggregate = summon[Monad[NValue]]

  object NValue:
    import NValueInternal.*
    given toNValue[A]: Conversion[A, NValue[A]] = apply(_)
    def apply[A](a: A): NValue[A] = FreeS.liftM(Concrete(NbrMap(a)))
    def nself[A](a: NValue[A]): NValue[A] = FreeS.liftM(Self(a))
    def nfold[A](init: A)(op: (A, A) => A)(a: NValue[A]): NValue[A] =
      FreeS.liftM(
        Builtin(a, d => domain => nv => NValue((domain - d).map(nv.concrete(using domain.map(d => d -> TEmpty()).toMap)(using d).get).foldLeft(init)(op))))
    extension [A](nv: NValue[A]) def selfValue: A = nself(nv).concrete(using Map.empty)(using selfDevice).defaultValue

  private[scafi] object NValueInternal:
    def fromConcrete[A](nvc: MapWithDefault[Device, A]): NValue[A] = FreeS.liftM(Concrete(nvc))
    def apply[A](a: A, map: Map[Device, A]): NValue[A] = FreeS.liftM(Concrete(NbrMap(a, map)))
    def localValue[B, A](nv: NValue[A]): Contextual[B, A] = nv.concrete.get(summon[Device])
    extension [A](nv: NValue[A])
      def defaultValue[B]: Contextual[B, A] = nv.concrete.defaultValue
      def concrete[B]: Contextual[B, NbrMap[A]] = nv match
        case Pure(a) => NbrMap(a)
        case Suspend(Concrete(nvc)) => nvc
        case Suspend(Self(nv2)) => NbrMap(nv2.concrete.get(summon[Device]))
        case Suspend(Builtin(nv2, f)) => f(summon[Device])(summon[Environment[B]].keySet)(nv2).concrete
        case FlatMap(nv2, f) => nv2.concrete.flatMap(x => f(x).concrete)