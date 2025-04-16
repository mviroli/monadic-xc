package scafi


trait Monad[M[_]]:
  def pure[A](a: A): M[A]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

object Monad:
  def apply[M[_]](using monad: Monad[M]): Monad[M] = monad

trait ~>[F[_], G[_]]:
  def apply[A](fa: F[A]): G[A]

trait Free[M[_], A]:

  import Free.*
  def flatMap[B](f: A => Free[M, B]): Free[M, B] = FlatMap(this, f)
  def map[B](f: A => B): Free[M, B] = flatMap(a => pure(f(a)))
  def foldMap[G[_] : Monad](natTrans: M ~> G): G[A] = this match
    case Pure(a) => Monad[G].pure(a)
    case Suspend(ma) => natTrans.apply(ma)
    case FlatMap(fa, f) => // need a G[B]
      Monad[G].flatMap(fa.foldMap(natTrans))(a => f(a).foldMap(natTrans))

object Free:
  def pure[M[_], A](a: A): Free[M, A] = Pure(a)
  def liftM[M[_], A](ma: M[A]): Free[M, A] = Suspend(ma)
  case class Pure[M[_], A](a: A) extends Free[M, A]
  case class FlatMap[M[_], A, B](fa: Free[M, A], f: A => Free[M, B]) extends Free[M, B]
  case class Suspend[M[_], A](ma: M[A]) extends Free[M, A]

enum Optional[+A]:
  case Some(a: A)
  case None
  def map[B](f: A => B): Optional[B] = this match
    case Some(a) => Some(f(a))
    case None => None

import Optional.*

type OptionalMonad[A] = Free[Optional, A]
def some[A](a: A): OptionalMonad[A] = Free.liftM(Some(a))
def none: OptionalMonad[Nothing] = Free.liftM(None)

def rand: OptionalMonad[Double] = Free.liftM:
    val r = Math.random()
    if r > 0.9 then None else Some(r)

def three: OptionalMonad[Double] =
  for
    a <- some(true)
    b <- some(20)
    c <- some(30)
  yield if a then b else c

// What does it mean to "execute" a flatmap?
// To get an Optional back from OptionalMonad

def back[A](om: OptionalMonad[A]): Optional[A] = om match
  case Free.Suspend(ma) => ma
  case Free.Pure(a) => Some(a)
  case Free.FlatMap(om2, f) => back(om2) match
    case None => None
    case Some(a) => back(f(a))

@main def play =
  println(back(three))