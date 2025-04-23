package scafi

object NValueLib:
  import NValueAggregates.{*, given}

  def mux[A](b: Aggregate[NValue[Boolean]])(th: Aggregate[A])(el: Aggregate[A]): Aggregate[A] =
    for
      cond <- b
      t <- th
      e <- el
    yield if cond.self then t else e



  /*
  def branch[A](cond: Aggregate[Boolean])(th: Aggregate[A])(el: Aggregate[A]): Aggregate[A] =
    call:
      mux(cond)(() => th)(() => el)
  */

