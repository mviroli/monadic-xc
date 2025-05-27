package scafi

case class MapWithDefault[K, +V] private(defaultValue: V, baseMap: Map[K, V]):
  override def toString: String = defaultValue.toString + "[" + baseMap.mkString(", ") + "]"
  //def flatMap[B](f: V => MapWithDefault[K, B]): MapWithDefault[K, B] =
  //  MapWithDefault(f(a).a, (map.keySet ++ f(a).map.keySet).map(k => (k, f(this.get(k)).get(k))).toMap)
  //def map[B](f: V => B): MapWithDefault[K, B] = MapWithDefault(f(this.a), map.map((k, v) => (k, f(v))))
  def get(d: K): V = baseMap.getOrElse(d, defaultValue)
  def asValue: V = if baseMap.isEmpty || baseMap.values.forall(v => v == defaultValue) then defaultValue else throw new MatchError(this)

object MapWithDefault:
  def apply[K, V](a: V): MapWithDefault[K, V] = new MapWithDefault(a, Map.empty)
  def apply[K, V](a: V, map: Map[K, V]): MapWithDefault[K, V] = new MapWithDefault(a, map.filterNot((_, v) => v == a))
  extension [K, V](a: V) def |>(e: (K, V)*): MapWithDefault[K, V] = apply(a, e.toMap)

import SMonads.*

given [K]: Monad[[X] =>> MapWithDefault[K, X]] with
  override def pure[A](a: A): MapWithDefault[K, A] = MapWithDefault[K, A](a)
  extension [A](ma: MapWithDefault[K, A])
    override def flatMap[B](f: A => MapWithDefault[K, B]): MapWithDefault[K, B] =
      MapWithDefault(f(ma.defaultValue).defaultValue, (ma.baseMap.keySet ++ f(ma.defaultValue).baseMap.keySet).map(k => (k, f(ma.get(k)).get(k))).toMap)
