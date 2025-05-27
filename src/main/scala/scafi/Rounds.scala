package scafi

import scala.reflect.ClassTag

object Rounds:
  import Devices.*
  import NValues.{*, given}
  import Aggregates.{*, given}
  type NbrMap[+A] = MapWithDefault[Device, A]
  def NbrMap[A](a: A, map: Map[Device, A] = Map.empty) = MapWithDefault(a, map)

  enum Tree[A]:
    case TVal(res: NbrMap[A])
    case TNext(left: Tree[Any], right: Tree[A]) extends Tree[A]
    case TCall(fun: NbrMap[() => Aggregate[Any]], nest: Tree[A])
    case TXc(ret: Tree[A], send: Tree[A])
    case TEmpty()

    def top: NbrMap[A] = this match
      case TVal(a) => a
      case TNext(_, r) => r.top
      case TCall(_, n) => n.top
      case TXc(ret, _) => ret.top
  export Tree.*

  object Contexts:
    type Environment[A] = Map[Device, Tree[A]]
    def localEnv[A](using Device)(t: Tree[A]): Environment[A] = Map(summon[Device] -> t)
    def restrictEnv[A](c: Environment[A])(domain: Domain): Environment[A] = c.filter((d, _) => domain.contains(d))

    def Env[A: Environment]: Environment[A] = summon[Environment[A]]

    extension [A](c: Environment[A])
      def as[W]: Environment[W] = c.asInstanceOf[Environment[W]]

    extension [A, W](c: Environment[A])
      def enter[B: ClassTag](f: B => Any, p: B => Boolean = (b: B) => true): Environment[W] =
        c.collectValues:
          case v: B if p(v) => f(v).asInstanceOf[Tree[W]]


    extension[K, V] (c: Map[K, V] )
      def collectValues[W](pf: PartialFunction[V, W]): Map[K, W] =
        c.collect:
          case device -> v if pf.isDefinedAt(v) => device -> pf(v)

  export Contexts.*
  type Contextual[A, B] = Environment[A] ?=> Device ?=> B
  type Round[A] = Contextual[A, Tree[A]]
  type RoundNV[A] = Contextual[Any, NbrMap[A]]

