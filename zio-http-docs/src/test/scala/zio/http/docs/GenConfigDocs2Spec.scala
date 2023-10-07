package zio.http.docs

import zio.Config
import zio.test._

object GenConfigDocs2Spec extends ZIOSpecDefault {

  def spec = suite("GenConfigDocs") {
    test("Single Config") {
      val conf     = Config.string("str")
      val expected =
        s"""| Field | Type |
           ^|-------|------|
           ^| str   | Text |
           ^""".stripMargin('^').trim

      assertTrue(GenConfigDocs2.gen(conf).toDocusaurusMarkdown == expected)
    }
  }
}
