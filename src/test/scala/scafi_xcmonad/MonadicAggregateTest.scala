package scafi_xcmonad

import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers.*
import scafi.facadeMonadXC.AggregateApplicativeSyntax.*
import scafi.facadeMonadXC.AggregateEngineModule.{*, given}
import scafi.facadeMonadXC.AggregateLib.*
import scafi.facadeMonadXC.Executor.DistributedSystem.platformSensor
import scafi.facadeMonadXC.Executor.{*, given}
import scafi.utils.MapWithDefault

class MonadicAggregateTest extends org.scalatest.funsuite.AnyFunSuite:

  def toAggr[A](a: A | NValue[A]): XC[A] = a match
    case a: A => compute(a)
    case nv: NValue[A] => fromNValue(nv)

  test("left identity"):
    val ag1: XC[Int] = fromNValue(0).flatMap(nv => rep(nv.selfValue)(identity))
    val ag2: XC[Int] = rep(fromNValue(0).selfValue)(identity)





