package scafi

object NValues:

  import Devices.*
  export NValue.{given, *}
  import FreeMonads.*

  case class NValueConcrete[+A] private[scafi] (a: A, map: Map[Device, A]):
    override def toString: String = a.toString + "[" + map.mkString(", ") + "]"
    def flatMap[B](f: A => NValueConcrete[B]): NValueConcrete[B] =
      NValueConcrete(f(a).a, (map.keySet ++ f(a).map.keySet).map(k => (k, f(this.get(k)).get(k))).toMap)
    def map[B](f: A => B): NValueConcrete[B] = NValueConcrete(f(this.a), map.map((k, v) => (k, f(v))))
    def get(d: Device): A = map.getOrElse(d, a)

  object NValueConcrete:
    def apply[A](a: A): NValueConcrete[A] = new NValueConcrete(a, Map.empty)
    def apply[A](a: A, map: Map[Device, A]): NValueConcrete[A] = new NValueConcrete(a, map.filterNot((_, v) => v == a))
    extension [A](a: A) def |>(e: (Device, A)*): NValueConcrete[A] = apply(a, e.toMap)

  enum NValueAST[A]:
    case Concrete(nvc: NValueConcrete[A])
    case Self(nv: NValue[A])
  import NValueAST.*

  type NValue[A] = Free[NValueAST, A]

  object NValue:
    import NValueInternal.*
    given g[A]: Conversion[A, NValue[A]] = apply(_)
    extension [A](a: A) def nv: NValue[A] = g(a)
    def apply[A](a: A): NValue[A] = Free.liftM(Concrete(NValueConcrete(a, Map.empty)))
    def self[A](a: NValue[A]): NValue[A] = Free.liftM(Self(a))
    extension [A](nv: NValue[A])
      def localValue(using Device): A = nv.concrete.get(summon[Device])

  private[scafi] object NValueInternal:
    def fromConcrete[A](nvc: NValueConcrete[A]): NValue[A] = Free.liftM(Concrete(nvc))
    def apply[A](a: A, map: Map[Device, A]): NValue[A] = Free.liftM(Concrete(NValueConcrete(a, map)))
    def local[A](a: NValue[A]): A = self(a).defaultValue
    extension [A](nv: NValue[A])
      def defaultValue: A = nv match
        case Free.Pure(a) => a
        case Free.Suspend(Concrete(NValueConcrete(a, _))) => a
        case Free.Suspend(Self(nv)) => nv.defaultValue
        case Free.FlatMap(nv, f) => f(nv.defaultValue).defaultValue // HERE'S THE LOOP!
      def localValue(using Device): A = concrete.get(summon[Device])
      def concrete(using Device): NValueConcrete[A] = nv match
        case Free.Pure(a) => NValueConcrete(a)
        case Free.Suspend(Concrete(nvc)) => nvc
        case Free.Suspend(Self(nv)) => NValueConcrete(nv.concrete.get(summon[Device]))
        case Free.FlatMap(nv, f) => nv.concrete.flatMap(x => f(x).concrete)