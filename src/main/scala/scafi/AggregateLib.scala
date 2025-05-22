package scafi

object AggregateLib:
  import Aggregates.{*, given}

  def self[A](ag: Aggregate[A]): Aggregate[A] = for v <- ag yield nself(v)

  def mux[A](b: Aggregate[Boolean])(th: Aggregate[A])(el: Aggregate[A])(using Device): Aggregate[A] =
    import NValues.NValueInternal.* // neede for nv, TO DROP!
    for
      cond <- b
      localCondition <- compute(nself(cond))
      t <- th
      e <- el
    yield if localCondition == true.nv then t else e
    //yield if cond.selfHasValue(true) then t else e


  def branch[A](cond: Aggregate[Boolean])(th: Aggregate[A])(el: Aggregate[A])(using Device): Aggregate[A] =
    call:
      mux(cond)(() => th)(() => el)

