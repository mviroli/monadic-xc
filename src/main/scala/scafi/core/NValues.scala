package scafi.core

import smonads.FreeSMonads.*
import FreeS.*

/**
 * DSL for Values, using the FreeMonad approach
 */

object NValues:

  export NValue.{given, *}
  import Environments.*

  enum NValueAST[A]:
    case Concrete(nvc: NbrMap[A])
    case Builtin(a: NValue[A], f: Device => Set[Device] => NValue[A] => NValue[A])
  import NValueAST.*

  type NValue[A] = Free[NValueAST, A]
  val nvalueAggregate = summon[Monad[NValue]]

  object NValue:
    import NValueInternal.*
    given toNValue[A]: Conversion[A, NValue[A]] = apply(_)
    def apply[A](a: A): NValue[A] = FreeS.liftM(Concrete(NbrMap(a)))
    def nself[A](a: NValue[A]): NValue[A] = FreeS.liftM(
      Builtin(a, d => domain => nv => NValue(nv.asNbrMap(using domain.map(d => d -> TEmpty()).toMap)(using d).get(d))))
    def nfold[A](init: A)(op: (A, A) => A)(a: NValue[A]): NValue[A] =
      FreeS.liftM(
        Builtin(a, d => domain => nv => NValue((domain - d).map(nv.asNbrMap(using domain.map(d => d -> TEmpty()).toMap)(using d).get).foldLeft(init)(op))))
    extension [A](nv: NValue[A]) def selfValue: A = nself(nv).asNbrMap(using Map.empty)(using selfDevice).defaultValue

  private[scafi] object NValueInternal:
    def fromNbrMap[A](nvc: NbrMap[A]): NValue[A] = FreeS.liftM(Concrete(nvc))
    def apply[A](a: A, map: Map[Device, A]): NValue[A] = FreeS.liftM(Concrete(NbrMap(a, map)))
    def localValue[B, A](nv: NValue[A]): Contextual[B, A] = nv.asNbrMap.get(summon[Device])
    extension [A](nv: NValue[A])
      def defaultValue[B]: Contextual[B, A] = nv.asNbrMap.defaultValue
      def asNbrMap[B]: Contextual[B, NbrMap[A]] = nv match
        case Pure(a) => NbrMap(a)
        case Suspend(Concrete(nvc)) => nvc
        case Suspend(Builtin(nv2, f)) => f(summon[Device])(summon[Environment[B]].keySet)(nv2).asNbrMap
        case FlatMap(nv2, f) => nv2.asNbrMap.flatMap(x => f(x).asNbrMap)