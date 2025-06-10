package scafi.core

import smonads.FreeSMonads.*
import FreeS.*

/**
 * DSL for Values, using the FreeMonad approach
 */

object NValueConstructs:

  export NValue.{given, *}
  import Environments.*

  enum NValueAST[A]:
    case Concrete(nvc: NbrMap[A])
    case Builtin(a: NValue[A], f: Device => Set[Device] => NbrMap[A] => NbrMap[A])
  import NValueAST.*

  type NValue[A] = Free[NValueAST, A]
  val nvalueAggregate = summon[Monad[NValue]]

  object NValue:
    given toNValue[A]: Conversion[A, NValue[A]] = apply(_)
    def apply[A](a: A): NValue[A] = FreeS.Pure(a)
    private[scafi] def apply[A](nvc: NbrMap[A]): NValue[A] = FreeS.liftM(Concrete(nvc))

    def nself[A](a: NValue[A]): NValue[A] = FreeS.liftM(
      Builtin(a, d => domain => nm => NbrMap(nm.get(d))))
    def nfold[A](init: A)(op: (A, A) => A)(a: NValue[A]): NValue[A] = FreeS.liftM(
        Builtin(a, d => domain => nm => NbrMap((domain - d).map(nm.get(_)).foldLeft(init)(op))))
    extension [A](nv: NValue[A]) def selfValue: A =
      import NValueSemantics.asNbrMap
      nself(nv).asNbrMap(using Map.empty)(using selfDevice).defaultValue
