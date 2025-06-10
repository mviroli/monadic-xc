package scafi.core

import scafi.utils.MapWithDefault
import scala.reflect.ClassTag

/**
 * Trees, Exports, Maps, Rounds
 */

object Environments:
  export Devices.*
  export Tree.*
  export Environment.*

  object Devices:
    opaque type Device = Int
    val selfDevice: Device = 0
    type Domain = Set[Device]
    private var counter: Device = 1

    def newDevice(): Device = try
      counter
    finally
      counter = counter + 1

  type NbrMap[+A] = MapWithDefault[Device, A]
  def NbrMap[A](a: A, map: Map[Device, A] = Map.empty): NbrMap[A] = MapWithDefault(a, map)

  enum Tree[+A]:
    case TVal(res: NbrMap[A])
    case TNext(left: Tree[Any], right: Tree[A]) extends Tree[A]
    case TCall(fun: NbrMap[Any], nest: Tree[A])
    case TXc(ret: Tree[A], send: Tree[A])
    case TEmpty()

    def top: NbrMap[A] = this match
      case TVal(a) => a
      case TNext(_, r) => r.top
      case TCall(_, n) => n.top
      case TXc(ret, _) => ret.top

  type Environment[A] = Map[Device, Tree[A]]
  object Environment:
    def localEnv[A](using Device)(t: Tree[A]): Environment[A] = Map(summon[Device] -> t)
    def restrictEnv[A](c: Environment[A])(domain: Domain): Environment[A] = c.filter((d, _) => domain.contains(d))
    def Env[A: Environment]: Environment[A] = summon[Environment[A]]
    extension [A, W](c: Environment[A])
      def enter[B: ClassTag](f: B => Any, p: B => Boolean = (b: B) => true): Environment[W] = c.collect:
          case device -> (v: B) if p(v) => device -> f(v).asInstanceOf[Tree[W]]

  type Contextual[A, B] = Environment[A] ?=> Device ?=> B
  type Round[A] = Contextual[A, Tree[A]]
  type RoundNV[A] = Contextual[Any, NbrMap[A]]