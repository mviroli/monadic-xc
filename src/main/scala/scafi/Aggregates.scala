package scafi

import scala.reflect.ClassTag

object Aggregates:
  import CFreeMonads.*
  export NValues.{*, given}
  export Devices.*
  export Aggregate.{*, given}

  enum AggregateAST[A]:
    case Val(a: () => NValue[A])
    case Builtin(a: Aggregate[A], f: Device => Set[Device] => NValue[A] => NValue[A])
    case Call(f: Aggregate[() => Aggregate[A]])
    case Xc(a: NValue[A], f: NValue[A] => (Aggregate[A], Aggregate[A]))

  import AggregateAST.*

  type Aggregate[A] = CFree[AggregateAST, NValue, A]
  type Contextual[A] = Device ?=> A

  object Aggregate:
    given [A]: Conversion[A, Aggregate[A]] = compute
    given [A]: Conversion[NValue[A], Aggregate[A]] = compute

    def compute[A](a: =>NValue[A]): Aggregate[A] = CFree.liftM(Val(() => a))
    def call[A](f: Aggregate[() => Aggregate[A]]): Aggregate[A] = CFree.liftM(Call(f))
    def exchange[A](a: Aggregate[A])(f: NValue[A] => (Aggregate[A], Aggregate[A])): Aggregate[A] =
      for
        v <- a
        e <- CFree.liftM(Xc(v, f))
      yield e
    def retsend[A](a: Aggregate[A])(f: NValue[A] => Aggregate[A]): Aggregate[A] = exchange(a)(v => (f(v), f(v)))

    def rep[A](a: NValue[A])(f: NValue[A] => Aggregate[A]): Contextual[Aggregate[A]] = retsend(compute(a))(x => f(self(x)))
    def self[A](a: NValue[A]): Contextual[NValue[A]] = NValue(local(a))
    def local[A](a: NValue[A]): Contextual[A] = a.get(summon[Device])
    def fold[A](init: A)(op: (A, A) => A)(a: Aggregate[A]): Aggregate[A] =
      CFree.liftM(Builtin(a, d => domain => nv => (domain - d).map(nv.get).foldLeft(init)(op)))
  export Semantics.*

  /*
    TODOs:
    - address contextuality for fold
    - use flatMap more thoroughly
    - build mini-simulator
   */




@main def testNFwF =
  import Aggregates.{*, given}

  def ag1: Aggregate[Int] = 10
  def ag2: Aggregate[Int] = 20
  def ag3 = for
    a1 <- ag1
    a2 <- ag2
  yield for
    i1 <- a1
    i2 <- a2
  yield i1 + i2
  given Device = selfDevice
  //println(ag3.round(local(TEmpty())))

@main def playNFwF =
  import Aggregates.{*, given}
  given Device = selfDevice
  def counter = rep(0)(for i <- _ yield i + 1)
  def ag = for
    a1 <- rep(0)(for i <- _ yield i + 1)
    a2 <- rep(0)(for i <- _ yield i + 1)
  yield for
    i1 <- a1
    i2 <- a2
  yield i1 + i2
  def caller = call(() => counter)

@main def playWrongCall =
  import Aggregates.{*, given}
  given Device = selfDevice
  def counter: Aggregate[() => Aggregate[Int]] = rep(() => retsend(0)(identity))(identity)
  def ag = for
    nf <- counter
  yield for
    f <- nf
  yield f()

@main def playWithSelf =
  import Aggregates.{*, given}
  given Device = selfDevice
  def counter = rep(0)(for i <- _ yield i + 1)
  def ag = self(counter)

