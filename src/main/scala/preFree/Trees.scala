package preFree

import others.AggregateFramework

object Trees:
  export Tree.*

  enum Tree[A]:
    case Rep(res: A, nest: Tree[A])
    case Val(res: A)
    case Next[B, A](left: Tree[B], right: Tree[A]) extends Tree[A]
    case Call(fun: Tree[() => AggregateFramework.Aggregate[A]], nest: Tree[A])
    case Empty()

    def top: A = this match
      case Rep(a, _) => a
      case Val(a) => a
      case Next(_, r) => r.top
      case Call(_, n) => n.top

