package scafi

case class MapWithDefault[K, +V] private(a: V, map: Map[K, V]):
  override def toString: String = a.toString + "[" + map.mkString(", ") + "]"
  def flatMap[B](f: V => MapWithDefault[K, B]): MapWithDefault[K, B] =
    MapWithDefault(f(a).a, (map.keySet ++ f(a).map.keySet).map(k => (k, f(this.get(k)).get(k))).toMap)
  def map[B](f: V => B): MapWithDefault[K, B] = MapWithDefault(f(this.a), map.map((k, v) => (k, f(v))))
  def get(d: K): V = map.getOrElse(d, a)
  def asValue: V = if map.isEmpty || map.values.forall(v => v == a) then a else throw new MatchError(this)

object MapWithDefault:
  def apply[K, V](a: V): MapWithDefault[K, V] = new MapWithDefault(a, Map.empty)
  def apply[K, V](a: V, map: Map[K, V]): MapWithDefault[K, V] = new MapWithDefault(a, map.filterNot((_, v) => v == a))
  extension [K, V](a: V) def |>(e: (K, V)*): MapWithDefault[K, V] = apply(a, e.toMap)
