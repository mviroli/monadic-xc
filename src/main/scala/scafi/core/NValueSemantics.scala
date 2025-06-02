package scafi.core

import scala.reflect.ClassTag

import smonads.FreeSMonads.*

object NValueSemantics:
  import Environments.*
  import NValues.{*, given}

  given Monad[RoundNV] with
    def pure[A](a: A): RoundNV[A] = NbrMap(a)
    extension [A](ma: RoundNV[A]) def flatMap[B](f: A => RoundNV[B]): RoundNV[B] =
      ma(using Env.as[Any]).flatMap(a => f(a))

  import NValueAST.*

  extension [B](env: Environment[B])
    private[scafi] def localInterpreted[A](nv: NValue[A]): Contextual[Any, A] =
      nv.foldMap[RoundNV](compilerNV).get(summon[Device])

  private def compilerNV: NValueAST ~~> RoundNV = new(NValueAST ~~> RoundNV):
    override def apply[A]: NValueAST[A] => RoundNV[A] =
      case Concrete(c) => c
      case Self(nv) => NbrMap(summon[Environment[Any]].localInterpreted(nv))
