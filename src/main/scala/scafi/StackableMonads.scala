package scafi

object StackableMonads:

  type Monad[M[_]] = StackableMonad[M, [X] =>> X]

  trait StackableMonad[M[_], C[_]: Monad]:
    def pure[A](a: C[A]): M[A]
    extension [A](ma: M[A])
      def flatMap[B](f: C[A] => M[B]): M[B]
      def map[B](f: C[A] => C[B]): M[B] = flatMap(x => pure(f(x)))
      def deepMap[B](f: A => B): M[B] = map(ca => ca.map(f))

  given Monad[[X] =>> X] with
    override def pure[A](a: A): A = a
    extension [A](ma: A)
      override def flatMap[B](f: A => B): B = f(ma)

object StackedMonadsExample:
  import StackableMonads.{*, given}
  enum Maybe[A]:
    case Some(a: A)
    case None()
  import Maybe.*

  enum SequenceOMaybes[A]:
    case Cons(h: Maybe[A], t: SequenceOMaybes[A])
    case Nil()
  import SequenceOMaybes.*
  object SequenceOMaybes:
    extension [A](seq: SequenceOMaybes[A])
      def concat(s2: SequenceOMaybes[A]): SequenceOMaybes[A] = seq match
        case Cons(h, t) => Cons(h, t.concat(s2))
        case _ => s2

  given Monad[Maybe] with
    override def pure[A](a: A): Maybe[A] = Some(a)
    extension [A](ma: Maybe[A])
      override def flatMap[B](f: A => Maybe[B]): Maybe[B] = ma match
        case Some(a) => f(a)
        case _ => None()

  given StackableMonad[SequenceOMaybes, Maybe] with
    override def pure[A](a: Maybe[A]): SequenceOMaybes[A] = Cons(a, Nil())

    extension [A](ma: SequenceOMaybes[A])
      override def flatMap[B](f: Maybe[A] => SequenceOMaybes[B]): SequenceOMaybes[B] = ma match
        case Cons(h, t) => f(h).concat(t.flatMap(f))
        case _ => Nil()

  @main def trySM =
    println:
      for
        m <- Cons(Some(5), Cons(Some(4), Nil()))
        n <- Cons(Some("a"), Cons(Some("b"), Nil()))
      yield for
        i <- m
        j <- n
      yield i + j


    println:
      Cons(Some(5), Cons(Some(4), Nil())).deepMap(_ + 1)
