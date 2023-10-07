package zio.http.docs

import zio.{Chunk, Config}
import zio.test._

object GenConfigDocs2Spec extends ZIOSpecDefault {

  def spec = suite("GenConfigDocs")(
    suite("basic")(
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
      test("Described") {
        case class Foo(str: String, num: Int)
        val conf =
          (Config.string("str") ++ Config.int("num").??("it's a number")).map { case (str, num) =>
            Foo(str, num)
          }

        val expected = {
          Table(
            Chunk(
              Row("str", "Text"),
              Row("num", "Integer", description = Some("it's a number")),
            ),
          )
        }

        assertTrue(GenConfigDocs2.gen(conf) == expected)
      },
      test("Described") {
        case class Foo(str: String, num: Int)
        val conf =
          (Config.string("str") ++ Config.int("num").??("it's a number")).map { case (str, num) =>
            Foo(str, num)
          }

        val expected = {
          Table(
            Chunk(
              Row("str", "Text"),
              Row("num", "Integer", description = Some("it's a number")),
            ),
          )
        }

        assertTrue(GenConfigDocs2.gen(conf) == expected)
      },
      test("Optional") {
        case class Foo(str: String, num: Option[Int])
        val conf =
          (Config.string("str") ++ Config.int("num").optional).map { case (str, num) =>
            Foo(str, num)
          }

        val expected = {
          Table(
            Chunk(
              Row("str", "Text"),
              Row("num", "Integer", optional = true),
            ),
          )
        }

        assertTrue(GenConfigDocs2.gen(conf) == expected)
      },
    ),
    suite("variants")(
      test("Default value") {
        case class Foo(str: String, num: Int)

        val conf =
          (Config.string("str") ++ Config.int("num").withDefault(1)).map { case (str, num) =>
            Foo(str, num)
          }

        val expected = {
          Table(
            Chunk(
              Row("str", "Text"),
              Row("num", "Integer", default = Some(1)),
            ),
          )
        }

        assertTrue(GenConfigDocs2.gen(conf) == expected)
      },
    ),
  )

}
