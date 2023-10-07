package zio.http.docs

import zio.test._

object GenConfigDocsSpec extends ZIOSpecDefault {

  def spec = suite("GenConfigDocs")(
    test("Server.Config") {
      val doc      = GenConfigDocs.serverConfigTable
      val expected =
        scala.io.Source.fromResource("server.md").getLines().mkString("\n").trim

      List(
        zio.http.Server.Config.config,
        zio.http.Client.Config.config,
        zio.http.netty.NettyConfig.config,
        zio.http.DnsResolver.Config.config,
      ).foreach { c =>
        val tbl = GenConfigDocs.genTable(c)
        println(tbl)
        println()
      }
//      GenConfigDocs.getFields(zio.http.Client.Config.config, zio.Chunk.empty).collectFirst {
//        case f if f.name == "connection-pool" => f.variants.foreach(println)
//      }

      assertTrue(doc == expected)
    },
  )

}
