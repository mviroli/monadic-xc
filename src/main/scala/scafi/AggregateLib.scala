package scafi

object AggregateLib:
  import Aggregates.{*, given}

  def retsend[A](a: Aggregate[A])(f: NValue[A] => Aggregate[A]): Aggregate[A] = exchange(a)(v => (f(v), f(v)))

  def rep[A](a: Aggregate[A])(f: NValue[A] => Aggregate[A]): Aggregate[A] = retsend(a)(x => f(nself(x)))

  def counter(initial: Int) = rep(initial)(for i <- _ yield i + 1)

  def self[A](ag: Aggregate[A]): Aggregate[A] = for v <- ag yield nself(v)

  def mux[A](b: Aggregate[Boolean])(th: Aggregate[A])(el: Aggregate[A]): Aggregate[A] =
    import NValues.NValueInternal.* // TO DROP!
    for
      cond <- b
      localCondition <- compute(nself(cond))
      t <- th
      e <- el
    yield if localCondition == true.nv then t else e
    //yield if cond.selfHasValue(true) then t else e


  def branch[A](cond: Aggregate[Boolean])(th: Aggregate[A])(el: Aggregate[A]): Aggregate[A] =
    call:
      mux(cond)(() => th)(() => el)

