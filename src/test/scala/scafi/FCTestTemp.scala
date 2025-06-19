package scafi

import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers.*
import scafi.experiments.FCEngineModule.{*, given}
import scafi.facade.Executor.*
import scafi.utils.MapWithDefault

class FCTestTemp extends org.scalatest.funsuite.AnyFunSuite:

  test("Count neighbours"):
    println()
