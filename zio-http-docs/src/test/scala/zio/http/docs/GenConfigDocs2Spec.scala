package zio.http.docs

import zio.{Chunk, Config}
import zio.test._

object GenConfigDocs2Spec extends ZIOSpecDefault {

  def spec = suite("GenConfigDocs")(
    suite("basic")(
      test("single") {
        val conf     = Config.string("str")
        val expected =
          Table(
            Chunk(
              Row("str", FieldType.Primitive("Text")),
            ),
          )

        assertTrue(GenConfigDocs2.gen(conf) == expected)
      },
      test("zipped") {
        val conf =
          Config.string("str") ++ Config.int("num")

        val expected = {
          Table(
            Chunk(
              Row("str", FieldType.Primitive("Text")),
              Row("num", FieldType.Primitive("Integer")),
            ),
          )
        }

        assertTrue(GenConfigDocs2.gen(conf) == expected)
      },
      test("zipped and mapped") {
        case class Foo(str: String, num: Int)
        val conf =
          (Config.string("str") ++ Config.int("num")).map { case (str, num) =>
            Foo(str, num)
          }

        val expected = {
          Table(
            Chunk(
              Row("str", FieldType.Primitive("Text")),
              Row("num", FieldType.Primitive("Integer")),
            ),
          )
        }

        assertTrue(GenConfigDocs2.gen(conf) == expected)
      },
      test("described") {
        case class Foo(str: String, num: Int)
        val conf =
          (Config.string("str") ++ Config.int("num").??("it's a number")).map { case (str, num) =>
            Foo(str, num)
          }

        val expected = {
          Table(
            Chunk(
              Row("str", FieldType.Primitive("Text")),
              Row("num", FieldType.Primitive("Integer"), description = Some("it's a number")),
            ),
          )
        }

        assertTrue(GenConfigDocs2.gen(conf) == expected)
      },
      test("described") {
        case class Foo(str: String, num: Int)
        val conf =
          (Config.string("str") ++ Config.int("num").??("it's a number")).map { case (str, num) =>
            Foo(str, num)
          }

        val expected = {
          Table(
            Chunk(
              Row("str", FieldType.Primitive("Text")),
              Row("num", FieldType.Primitive("Integer"), description = Some("it's a number")),
            ),
          )
        }

        assertTrue(GenConfigDocs2.gen(conf) == expected)
      },
      test("optional") {
        case class Foo(str: String, num: Option[Int])
        val conf =
          (Config.string("str") ++ Config.int("num").optional).map { case (str, num) =>
            Foo(str, num)
          }

        val expected = {
          Table(
            Chunk(
              Row("str", FieldType.Primitive("Text")),
              Row("num", FieldType.Primitive("Integer"), optional = true),
            ),
          )
        }

        assertTrue(GenConfigDocs2.gen(conf) == expected)
      },
    ),
    suite("variants")(
      test("default value") {
        case class Foo(str: String, num: Int)

        val conf =
          (Config.string("str") ++ Config.int("num").withDefault(1)).map { case (str, num) =>
            Foo(str, num)
          }

        val expected =
          Table(
            Chunk(
              Row("str", FieldType.Primitive("Text")),
              Row("num", FieldType.Primitive("Integer"), default = Some(1)),
            ),
          )

        assertTrue(GenConfigDocs2.gen(conf) == expected)
      },
      test("switch - constant") {
        case class Foo(str: String, num: Int)

        val numConf: Config[Int] =
          Config.string.switch(
            "one" -> Config.Constant(1),
            "two" -> Config.succeed(2),
          )

        val conf =
          (Config.string("str") ++ numConf.nested("num")).map { case (str, num) =>
            Foo(str, num)
          }

        val expected =
          Table(
            Chunk(
              Row("str", FieldType.Primitive("Text")),
              Row("num", FieldType.Enum(FieldType.Primitive("Text"), Map("one" -> 1, "two" -> 2))),
            ),
          )

        assertTrue(GenConfigDocs2.gen(conf) == expected)
      },
      test("switch - constant - inverted") {
        case class Foo(str: String, num: Int)

        val numConf: Config[Int] =
          Config
            .string("num")
            .switch(
              "one" -> Config.Constant(1),
              "two" -> Config.succeed(2),
            )

        val conf =
          (Config.string("str") ++ numConf).map { case (str, num) =>
            Foo(str, num)
          }

        val expected =
          Table(
            Chunk(
              Row("str", FieldType.Primitive("Text")),
              Row("num", FieldType.Enum(FieldType.Primitive("Text"), Map("one" -> 1, "two" -> 2))),
            ),
          )

        assertTrue(GenConfigDocs2.gen(conf) == expected)
      },
    ),
  )

}
