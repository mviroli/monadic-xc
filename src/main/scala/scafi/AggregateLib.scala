package scafi

object AggregateLib:
  import Aggregates.{*, given}

  def mux[A](b: Aggregate[Boolean])(th: Aggregate[A])(el: Aggregate[A])(using Device): Aggregate[A] =
    for
      cond <- b
      t <- th
      e <- el
    yield if self(cond) == true.nv then t else e


  def branch[A](cond: Aggregate[Boolean])(th: Aggregate[A])(el: Aggregate[A])(using Device): Aggregate[A] =
    call:
      mux(cond)(() => th)(() => el)

