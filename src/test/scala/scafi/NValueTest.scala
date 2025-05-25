package scafi

import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers.*
import NValues.{*, given}
import MapWithDefault.*

class MapWithDefaultTest extends org.scalatest.funsuite.AnyFunSuite:

  given [K, A]: Conversion[A, MapWithDefault[K, A]] = MapWithDefault(_, Map.empty)

  test("toString"):
    val v: MapWithDefault[Int, Int] = 5
    v.toString shouldBe "5[]"
    MapWithDefault("z", Map(0 -> "a", 1 -> "b")).toString shouldBe "z[0 -> a, 1 -> b]"
    ("z" |> (0 -> "a", 1 -> "b")).toString shouldBe "z[0 -> a, 1 -> b]"

  test("get"):
    MapWithDefault("z", Map(0 -> "a", 1 -> "b", 2 -> "c")).get(0) shouldBe "a"
    MapWithDefault("z", Map(0 -> "a", 1 -> "b", 2 -> "c")).get(1) shouldBe "b"
    MapWithDefault("z", Map(0 -> "a", 1 -> "b", 2 -> "c")).get(2) shouldBe "c"
    MapWithDefault("z", Map(0 -> "a", 1 -> "b", 2 -> "c")).get(3) shouldBe "z"

  //test("restrict"):
  //  NValue("z", Map(0 -> "a", 1 -> "b", 2 -> "c")).restrict(Set(1)) shouldBe NValue("z", Map(1 -> "b"))

  test("flatMapping"):
    val nv = MapWithDefault(5, Map(0 -> 10, 1 -> 11)).flatMap: x =>
      MapWithDefault(100 + x, Map(0 -> (1 + x), 2 -> x * x))
    nv shouldBe MapWithDefault(105, Map(0 -> 11, 1 -> 111, 2 -> 25))

  test("lifting"):
    val v: MapWithDefault[Int, Int] = for
      x <- MapWithDefault(5, Map(0 -> 10, 1 -> 11))
    yield x + 1
    v shouldBe MapWithDefault(6, Map(0 -> 11, 1 -> 12))

  test("composition"):
    val v: MapWithDefault[Int, String] = for
      x <- MapWithDefault(5, Map(0 -> 10, 1 -> 11))
      y <- MapWithDefault(6, Map(0 -> 100, 2 -> 200))
      z <- MapWithDefault("def", Map(0 -> "a"))
    yield x + y + z
    v shouldBe MapWithDefault("11def", Map(0 -> "110a", 1 -> "17def", 2 -> "205def"))

  test("composition explained"):
    val v: MapWithDefault[Int, Int] =
      MapWithDefault(5, Map(0 -> 10, 1 -> 11)).flatMap: x =>
        MapWithDefault(6, Map(0 -> 100, 2 -> 200)).map(y => x + y)
    v shouldBe MapWithDefault(11, Map(0 -> 110, 1 -> 17, 2 -> 205))

