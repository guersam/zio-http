package zio.http.docs

import zio.{Chunk, Config}
import zio.test._

object GenConfigDocs2Spec extends ZIOSpecDefault {

  def spec = suite("GenConfigDocs")(
    suite("basic")(
      test("single") {
        val conf     = Config.string("str")
        val expected =
          ConfigDoc.product(
            Row("str", FieldType.Primitive("Text")),
          )

        assertTrue(GenConfigDocs2.gen(conf) == expected)
      },
      test("zipped") {
        val conf =
          Config.string("str") ++ Config.int("num")

        val expected = {
          ConfigDoc.product(
            Row("str", FieldType.Primitive("Text")),
            Row("num", FieldType.Primitive("Integer")),
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
          ConfigDoc.product(
            Row("str", FieldType.Primitive("Text")),
            Row("num", FieldType.Primitive("Integer")),
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
          ConfigDoc.product(
            Row("str", FieldType.Primitive("Text")),
            Row("num", FieldType.Primitive("Integer"), description = Some("it's a number")),
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
          ConfigDoc.product(
            Row("str", FieldType.Primitive("Text")),
            Row("num", FieldType.Primitive("Integer"), description = Some("it's a number")),
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
          ConfigDoc.product(
            Row("str", FieldType.Primitive("Text")),
            Row("num", FieldType.Primitive("Integer"), optional = true),
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
          ConfigDoc.product(
            Row("str", FieldType.Primitive("Text")),
            Row("num", FieldType.Primitive("Integer"), default = Some(1)),
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
          ConfigDoc.product(
            Row("str", FieldType.Primitive("Text")),
            Row("num", FieldType.Enum(FieldType.Primitive("Text"), Map("one" -> 1, "two" -> 2))),
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
          ConfigDoc.product(
            Row("str", FieldType.Primitive("Text")),
            Row("num", FieldType.Enum(FieldType.Primitive("Text"), Map("one" -> 1, "two" -> 2))),
          )

        assertTrue(GenConfigDocs2.gen(conf) == expected)
      },
    ),
    test("Variant") {
      sealed trait Conf
      case class Foo(foo: String)  extends Conf
      case class Bar(bar: Int)     extends Conf
      case class Baz(baz: Boolean) extends Conf

      val conf: Config[Conf] =
        Config.string("foo").map(Foo) ||
          Config.int("bar").map(Bar) ||
          Config.boolean("baz").map(Baz)

      val expected =
        ConfigDoc.variants(
          ConfigDoc.product(
            Row("foo", FieldType.Primitive("Text")),
          ),
          ConfigDoc.product(
            Row("bar", FieldType.Primitive("Integer")),
          ),
          ConfigDoc.product(
            Row("baz", FieldType.Primitive("Bool")),
          ),
        )

      assertTrue(GenConfigDocs2.gen(conf) == expected)
    },
    test("Markdown") {
      println(
        GenConfigDocs2
          .gen(
            zio.http.Server.Config.config,
          )
          .toDocusaurusMarkdown,
      )

      assertCompletes
    },
  )

}
