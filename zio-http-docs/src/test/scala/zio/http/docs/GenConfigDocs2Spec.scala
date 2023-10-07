package zio.http.docs

import zio.{Chunk, Config}
import zio.test._

object GenConfigDocs2Spec extends ZIOSpecDefault {

  def spec = suite("GenConfigDocs")(
    test("Single") {
      val conf     = Config.string("str")
      val expected =
        Table(
          Chunk(
            Row("str", "Text"),
          ),
        )

      assertTrue(GenConfigDocs2.gen(conf) == expected)
    },
    test("Zipped") {
      val conf =
        Config.string("str") ++ Config.int("num")

      val expected = {
        Table(
          Chunk(
            Row("str", "Text"),
            Row("num", "Integer"),
          ),
        )
      }

      assertTrue(GenConfigDocs2.gen(conf) == expected)
    },
    test("Zipped and Mapped") {
      case class Foo(str: String, num: Int)
      val conf =
        (Config.string("str") ++ Config.int("num")).map { case (str, num) =>
          Foo(str, num)
        }

      val expected = {
        Table(
          Chunk(
            Row("str", "Text"),
            Row("num", "Integer"),
          ),
        )
      }

      assertTrue(GenConfigDocs2.gen(conf) == expected)
    },
  )

}
