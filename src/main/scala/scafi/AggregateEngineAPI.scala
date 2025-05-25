package scafi

trait AggregateEngineAPI:
  api: AggregateLanguageAPI =>

  type NValueConcrete[A]
  type Export[A] <: { def top: NValueConcrete[A] }
  def initialExport[A]: Export[A]

  type Device
  type Environment[A] = Map[Device, Export[A]]

  def newDevice(): Device
  def selfDevice: Device
  def round[A](a: Aggregate[A])(d: Device)(e: Environment[A]): Export[A]

class AggregateEngine extends AggregateEngineAPI with AggregateLanguage:
  export NValues.NValueConcrete
  type Export[A] = Rounds.Tree[A]
  override def initialExport[A]: Rounds.Tree[A] = Rounds.Tree.TEmpty[A]()

  export Devices.{Device, newDevice, selfDevice}

  import Semantics.round
  override def round[A](a: Aggregate[A])(d: Device)(e: Environment[A]): Rounds.Tree[A] = a.round(using d)(e)

object AggregateEngineModule extends AggregateEngine

