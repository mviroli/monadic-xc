package scafi.facade

import scafi.utils.MapWithDefault

trait AggregateEngineAPI:
  api: AggregateLanguageAPI =>

  type Export[A] <: { def top: MapWithDefault[Device, A] }
  def initialExport[A]: Export[A]

  type Device
  type Environment[A]

  def newDevice(): Device
  def selfDevice: Device
  def round[A](a: Aggregate[A])(d: Device)(e: Environment[A]): Export[A]

class AggregateEngine extends AggregateEngineAPI with AggregateLanguage:
  import scafi.core.*

  export Environments.Environment
  type Export[A] = Environments.Tree[A]
  override def initialExport[A]: Environments.Tree[A] = Environments.Tree.TEmpty[A]()

  export Devices.{Device, newDevice, selfDevice}

  import scafi.core.AggregateSemantics.round
  override def round[A](a: Aggregate[A])(d: Device)(e: Environment[A]): Environments.Tree[A] = a.round(using e)(using d)

object AggregateEngineModule extends AggregateEngine

