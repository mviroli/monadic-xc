package scafi

import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers.*
import NValues.{*, given}
import Devices.{*, given}

class NValueConcreteTest extends org.scalatest.funsuite.AnyFunSuite:

  val devs: Seq[Device] = Seq(selfDevice, Devices.newDevice(), Devices.newDevice(), Devices.newDevice())
  given [A]: Conversion[(Int, A), (Device, A)] = is => (devs(is._1), is._2)
  given Conversion[Int, Device] = devs.apply
  given [A]: Conversion[A, NValueConcrete[A]] = NValueConcrete(_, Map.empty)

  test("toString"):
    val v: NValueConcrete[Int] = 5
    v.toString shouldBe "5[]"
    NValueConcrete("z", Map(0 -> "a", 1 -> "b")).toString shouldBe "z[0 -> a, 1 -> b]"
    ("z" |> (0 -> "a", 1 -> "b")).concrete.toString shouldBe "z[0 -> a, 1 -> b]"

  test("get"):
    NValueConcrete("z", Map(0 -> "a", 1 -> "b", 2 -> "c")).get(0) shouldBe "a"
    NValueConcrete("z", Map(0 -> "a", 1 -> "b", 2 -> "c")).get(1) shouldBe "b"
    NValueConcrete("z", Map(0 -> "a", 1 -> "b", 2 -> "c")).get(2) shouldBe "c"
    NValueConcrete("z", Map(0 -> "a", 1 -> "b", 2 -> "c")).get(3) shouldBe "z"

  //test("restrict"):
  //  NValue("z", Map(0 -> "a", 1 -> "b", 2 -> "c")).restrict(Set(1)) shouldBe NValue("z", Map(1 -> "b"))

  test("flatMapping"):
    val nv = NValueConcrete(5, Map(0 -> 10, 1 -> 11)).flatMap: x =>
      NValueConcrete(100 + x, Map(0 -> (1 + x), 2 -> x * x))
    nv shouldBe NValueConcrete(105, Map(0 -> 11, 1 -> 111, 2 -> 25))

  test("lifting"):
    val v: NValueConcrete[Int] = for
      x <- NValueConcrete(5, Map(0 -> 10, 1 -> 11))
    yield x + 1
    v shouldBe NValueConcrete(6, Map(0 -> 11, 1 -> 12))

  test("composition"):
    val v: NValueConcrete[String] = for
      x <- NValueConcrete(5, Map(0 -> 10, 1 -> 11))
      y <- NValueConcrete(6, Map(0 -> 100, 2 -> 200))
      z <- NValueConcrete("def", Map(0 -> "a"))
    yield x + y + z
    v shouldBe NValueConcrete("11def", Map(0 -> "110a", 1 -> "17def", 2 -> "205def"))

  test("composition explained"):
    val v: NValueConcrete[Int] =
      NValueConcrete(5, Map(0 -> 10, 1 -> 11)).flatMap: x =>
        NValueConcrete(6, Map(0 -> 100, 2 -> 200)).map(y => x + y)
    v shouldBe NValueConcrete(11, Map(0 -> 110, 1 -> 17, 2 -> 205))

  //test("composition2"):
  //  val v1 = NValue(5, Map(0 -> 10, 1 -> 11))
  //  val v2 = NValue(6, Map(0 -> 100, 2 -> 200))
  //  v1.map2(v2)(_ + _) shouldBe NValue(11, Map(0 -> 110, 1 -> 17, 2 -> 205))


