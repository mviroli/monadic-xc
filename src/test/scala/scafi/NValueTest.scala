package scafi

import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers.*
import scafi.NValuesFramework.*


class NValueTest extends org.scalatest.funsuite.AnyFunSuite:

  test("toString"):
    val v: NValue[Int] = 5
    v.toString shouldBe "5[]"
    NValue("z", Map(0 -> "a", 1 -> "b")).toString shouldBe "z[0 -> a, 1 -> b]"
    ("z" |> (0 -> "a", 1 -> "b")).toString shouldBe "z[0 -> a, 1 -> b]"

  test("get"):
    NValue("z", Map(0 -> "a", 1 -> "b", 2 -> "c"))(0) shouldBe "a"
    NValue("z", Map(0 -> "a", 1 -> "b", 2 -> "c"))(1) shouldBe "b"
    NValue("z", Map(0 -> "a", 1 -> "b", 2 -> "c"))(2) shouldBe "c"
    NValue("z", Map(0 -> "a", 1 -> "b", 2 -> "c"))(3) shouldBe "z"

  test("restrict"):
    NValue("z", Map(0 -> "a", 1 -> "b", 2 -> "c")).restrict(Set(1)) shouldBe NValue("z", Map(1 -> "b"))

  test("flatMapping"):
    val nv = NValue(5, Map(0 -> 10, 1 -> 11)).flatMap: x =>
      NValue(100 + x, Map(0 -> (1 + x), 2 -> x * x))
    nv shouldBe NValue(105, Map(0 -> 11, 1 -> 111, 2 -> 25))

  test("lifting"):
    val v: NValue[Int] = for
      x <- NValue(5, Map(0 -> 10, 1 -> 11))
    yield x + 1
    v shouldBe NValue(6, Map(0 -> 11, 1 -> 12))

  test("composition"):
    val v: NValue[Int] = for
      x <- NValue(5, Map(0 -> 10, 1 -> 11))
      y <- NValue(6, Map(0 -> 100, 2 -> 200))
    yield x + y
    v shouldBe NValue(11, Map(0 -> 110, 1 -> 17, 2 -> 205))

  test("composition explained"):
    val v: NValue[Int] =
      NValue(5, Map(0 -> 10, 1 -> 11)).flatMap: x =>
        NValue(6, Map(0 -> 100, 2 -> 200)).map(y => x + y)
    v shouldBe NValue(11, Map(0 -> 110, 1 -> 17, 2 -> 205))

  test("composition2"):
    val v1 = NValue(5, Map(0 -> 10, 1 -> 11))
    val v2 = NValue(6, Map(0 -> 100, 2 -> 200))
    v1.map2(v2)(_ + _) shouldBe NValue(11, Map(0 -> 110, 1 -> 17, 2 -> 205))


