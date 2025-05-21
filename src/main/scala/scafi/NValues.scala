package scafi

object NValues:

  import Devices.*
  export NValue.{given, *}
  import FreeMonads.*

  trait NValueAST[+A]
  case class NValueConcrete[+A](a: A, map: Map[Device, A]) extends NValueAST[A]:
    override def toString: String = a.toString + "[" + map.mkString(", ") + "]"
    def flatMap[B](f: A => NValueConcrete[B]): NValueConcrete[B] =
      NValueConcrete(f(a).a, (map.keySet ++ f(a).map.keySet).map(k => (k, f(this.get(k)).get(k))).toMap)
    def map[B](f: A => B): NValueConcrete[B] = NValueConcrete(f(this.a), map.map((k, v) => (k, f(v))))
    def get(d: Device): A = map.getOrElse(d, a)

  object NValueConcrete:
    def apply[A](a: A, map: Map[Device, A]): NValueConcrete[A] = new NValueConcrete(a, map.filterNot((_, v) => v == a))

  type NValue[A] = Free[NValueConcrete, A]

  object NValue:
    given g[A]: Conversion[A, NValue[A]] = apply(_)
    extension [A](a: A) def nv: NValue[A] = g(a)
    def apply[A](a: A): NValue[A] = Free.Pure(a)
    def apply[A](a: A, map: Map[Device, A]): NValue[A] = Free.liftM(NValueConcrete(a, map))
    extension [A](a: A) def |>(e: (Device, A)*): NValue[A] = apply(a, e.toMap)

    extension [A](nv: NValue[A])
      def get(d: Device): A = nv.concrete.get(d)
      def concrete: NValueConcrete[A] = nv match
        case Free.Pure(a) => NValueConcrete(a, Map.empty)
        case Free.Suspend(ma) => ma
        case Free.FlatMap(nv, f) => nv.concrete.flatMap(x => f(x).concrete)
