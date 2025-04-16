package scafi

object NValuesFramework:

  import NValue.given
  export NValue.*

  type Device = Int
  type Domain = Set[Int]
  val selfDevice: Device = 0

  case class NValue[A](a: A, map: Map[Device, A]):
    override def toString: String = a.toString + "[" + map.mkString(", ") + "]"
    def flatMap[B](f: A => NValue[B]): NValue[B] =
      NValue(f(a).a, (map.keySet ++ f(a).map.keySet).map(k => (k, f(this(k))(k))).toMap)
    def map[B](f: A => B): NValue[B] = NValue(f(this.a), map.map((k, v) => (k, f(v))))
    def apply(d: Device): A = map.getOrElse(d, a)
    def restrict(dom: Domain): NValue[A] = NValue(a, map.filter((k, _) => dom.contains(k)))
    def cut(dom: Domain): NValue[A] = NValue(a, map.filterNot((k, _) => dom.contains(k)))
    def map2[B, C](other: NValue[B])(f: (A, B) => C): NValue[C] =
      NValue(f(this.a, other.a), (this.map.keySet ++ other.map.keySet).map(k => (k, f(this(k),other(k)))).toMap)

  object NValue:
    given g[A]: Conversion[A, NValue[A]] = NValue(_, Map.empty)
    extension [A](a: A) def |>(e: (Device, A)*): NValue[A] =
      NValue(a, e.toMap)

