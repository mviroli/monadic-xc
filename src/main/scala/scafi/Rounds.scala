package scafi

import scala.reflect.ClassTag

object Rounds:
  import Devices.*
  import NValues.{*, given}
  import Aggregates.{*, given}

  enum Tree[+A]:
    case TRep(res: NValue[A], nest: Tree[A])
    case TVal(res: NValue[A])
    case TSelf(res: NValue[A], nest: Tree[A])
    case TNext(left: Tree[Any], right: Tree[A]) extends Tree[A]
    case TCall(fun: Tree[() => Aggregate[Any]], nest: Tree[A])
    case TXc(init: Tree[A], ret: Tree[A], send: Tree[A])
    case TEmpty()

    def top: NValue[A] = this match
      case TRep(a, _) => a
      case TVal(a) => a
      case TSelf(a, _) => a
      case TNext(_, r) => r.top
      case TCall(_, n) => n.top
      case TXc(_, ret, _) => ret.top

  export Tree.*

  object Contexts:
    type Context[A] = Map[Device, Tree[A]]
    def localContext[A](using Device)(t: Tree[A]): Context[A] = Map(summon[Device] -> t)
    def restrict[A](c: Context[A])(domain: Domain): Context[A] = c.filter((d, _) => domain.contains(d))

    extension [A, W](c: Context[A])
      def enter[B: ClassTag](f: B => Any, p: B => Boolean = (b: B) => true): Context[W] =
        c.collectValues:
          case v: B if p(v) => f(v).asInstanceOf[Tree[W]]

    extension[K, V] (c: Map[K, V] )
      def collectValues[W](pf: PartialFunction[V, W]): Map[K, W] =
        c.collect:
          case device -> v if pf.isDefinedAt(v) => device -> pf(v)

  export Contexts.*
  type Round[A] = Device ?=> Context[A] => Tree[A]

