package scafi

object NValues:

  import Devices.*
  export NValue.{given, *}
  import FreeMonads.*

  case class NValueAST[+A](a: A, map: Map[Device, A]):
    override def toString: String = a.toString + "[" + map.mkString(", ") + "]"
    def flatMap[B](f: A => NValueAST[B]): NValueAST[B] =
      NValueAST(f(a).a, (map.keySet ++ f(a).map.keySet).map(k => (k, f(this.get(k)).get(k))).toMap)
    def map[B](f: A => B): NValueAST[B] = NValueAST(f(this.a), map.map((k, v) => (k, f(v))))
    def get(d: Device): A = map.getOrElse(d, a)

  type NValue[A] = Free[NValueAST, A]

  object NValue:
    given g[A]: Conversion[A, NValue[A]] = apply(_)
    extension [A](a: A) def nv: NValue[A] = g(a)
    def apply[A](a: A): NValue[A] = Free.Pure(a)
    def apply[A](a: A, map: Map[Device, A]): NValue[A] = Free.liftM(NValueAST(a, map.filterNot((_, v) => v == a)))
    extension [A](a: A) def |>(e: (Device, A)*): NValue[A] = apply(a, e.toMap)

    extension [A](nv: NValue[A])
      def concrete: NValueAST[A] = nv match
        case Free.Pure(a) => NValueAST(a, Map.empty)
        case Free.Suspend(ma) => ma
        case Free.FlatMap(nv, f) => nv.concrete.flatMap(x => f(x).concrete)
