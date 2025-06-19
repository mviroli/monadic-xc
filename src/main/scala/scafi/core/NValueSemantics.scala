package scafi.core

import smonads.FreeSMonads.*
import NValueConstructs.{*, given}
import Environments.*

/**
 * Semantics for NValues, using the FreeMonad approach
 * => it relies on utility MapWithDefault, aka NbrMap
 */

trait NValueSemanticsAPI:
  extension [A](nv: NValue[A])
    def asNbrMap[B]: Contextual[B, NbrMap[A]]
    def localValue: Contextual[Any, A]
  given monadRoundNV: Monad[RoundNV]


object NValueSemantics extends NValueSemanticsAPI:

  extension [A](nv: NValue[A])
    def asNbrMap[B]: Contextual[B, NbrMap[A]] =
      nv.foldMap[RoundNV](compilerNV)
    def localValue: Contextual[Any, A] =
      asNbrMap.get(summon[Device])

  given monadRoundNV: Monad[RoundNV] with
    def pure[A](a: A): RoundNV[A] = NbrMap(a)
    extension [A](ma: RoundNV[A]) def flatMap[B](f: A => RoundNV[B]): RoundNV[B] =
      summon[Monad[NbrMap]].flatMap(ma)(f(_))

  import NValueAST.*

  private def compilerNV: NValueAST ~~> RoundNV = new(NValueAST ~~> RoundNV):
    override def apply[A]: NValueAST[A] => RoundNV[A] =
      case Concrete(c) => c
      case Builtin(a, f) =>
        //println(s"out: $a ${Env} ${a.asNbrMap} ${f(summon[Device])(Env.keySet)(a.asNbrMap)}")
        f(summon[Device])(Env.keySet)(a.asNbrMap)
