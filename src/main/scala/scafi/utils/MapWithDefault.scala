package scafi.utils

import scafi.utils.MapWithDefault

case class MapWithDefault[K, +V] private(defaultValue: V, baseMap: Map[K, V]):
  override def toString: String = defaultValue.toString + "[" + baseMap.mkString(", ") + "]"
  def get(d: K): V = baseMap.getOrElse(d, defaultValue)
  def asValue: V = if baseMap.isEmpty then defaultValue else throw new MatchError(this)

object MapWithDefault:
  def apply[K, V](a: V): MapWithDefault[K, V] = new MapWithDefault(a, Map.empty)
  def apply[K, V](a: V, map: Map[K, V] = Map.empty): MapWithDefault[K, V] = new MapWithDefault(a, map.filterNot((_, v) => v == a))
  extension [K, V](a: V) def |>(e: (K, V)*): MapWithDefault[K, V] = apply(a, e.toMap)

  import fplib.SMonads.*
  given [K]: Monad[[X] =>> MapWithDefault[K, X]] with
    override def pure[A](a: A): MapWithDefault[K, A] = MapWithDefault[K, A](a)

    extension [A](ma: MapWithDefault[K, A])
      override def flatMap[B](f: A => MapWithDefault[K, B]): MapWithDefault[K, B] =
        MapWithDefault(f(ma.defaultValue).defaultValue, (ma.baseMap.keySet ++ f(ma.defaultValue).baseMap.keySet).map(k => (k, f(ma.get(k)).get(k))).toMap)


