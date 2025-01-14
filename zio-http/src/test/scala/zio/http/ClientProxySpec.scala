/*
 * Copyright 2021 - 2023 Sporta Technologies PVT LTD & the ZIO HTTP contributors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package zio.http

import java.net.ConnectException

import zio.test.Assertion._
import zio.test.TestAspect.{sequential, timeout}
import zio.test._
import zio.{Scope, ZIO, durationInt}

import zio.http.internal.{DynamicServer, HttpRunnableSpec, severTestLayer}
import zio.http.middleware.Auth.Credentials
import zio.http.model._
import zio.http.netty.client.NettyClientDriver

object ClientProxySpec extends HttpRunnableSpec {

  def clientProxySpec = suite("ClientProxySpec")(
    test("handle proxy connection failure") {
      val res =
        for {
          validServerPort <- ZIO.environmentWithZIO[DynamicServer](_.get.port)
          serverUrl       <- ZIO.fromEither(URL.fromString(s"http://localhost:$validServerPort"))
          proxyUrl        <- ZIO.fromEither(URL.fromString("http://localhost:0001"))
          out             <- Client
            .request(
              Request.get(url = serverUrl),
            )
            .provideSome(
              Client.live,
              ClientConfig.live(ClientConfig.empty.proxy(Proxy(proxyUrl))),
              NettyClientDriver.fromConfig,
            )
        } yield out
      assertZIO(res.either)(isLeft(isSubtype[ConnectException](anything)))
    },
    test("proxy respond Ok") {
      val res =
        for {
          port <- ZIO.environmentWithZIO[DynamicServer](_.get.port)
          url  <- ZIO.fromEither(URL.fromString(s"http://localhost:$port"))
          id   <- DynamicServer.deploy(Handler.ok.toHttp)
          proxy = Proxy.empty.withUrl(url).withHeaders(Headers(DynamicServer.APP_ID, id))
          out <- Client
            .request(
              Request.get(url = url),
            )
            .provideSome(
              Client.live,
              ClientConfig.live(ClientConfig.empty.proxy(proxy)),
              NettyClientDriver.fromConfig,
            )
        } yield out
      assertZIO(res.either)(isRight)
    },
    test("proxy respond Ok for auth server") {
      val proxyAuthApp = Handler
        .fromFunction[Request] { req =>
          val proxyAuthHeaderName = HeaderNames.proxyAuthorization.toString
          req.headers.toList.collectFirst { case Header(`proxyAuthHeaderName`, _) =>
            Response.ok
          }.getOrElse(Response.status(Status.Forbidden))
        }
        .toHttp

      val res =
        for {
          port <- ZIO.environmentWithZIO[DynamicServer](_.get.port)
          url  <- ZIO.fromEither(URL.fromString(s"http://localhost:$port"))
          id   <- DynamicServer.deploy(proxyAuthApp)
          proxy = Proxy.empty
            .withUrl(url)
            .withHeaders(Headers(DynamicServer.APP_ID, id))
            .withCredentials(Credentials("test", "test"))
          out <- Client
            .request(
              Request.get(url = url),
            )
            .provideSome(
              Client.live,
              ClientConfig.live(ClientConfig.empty.proxy(proxy)),
              NettyClientDriver.fromConfig,
            )
        } yield out
      assertZIO(res.either)(isRight)
    },
  )

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("ClientProxy") {
    serve(DynamicServer.app).as(List(clientProxySpec))
  }.provideShared(DynamicServer.live, severTestLayer) @@
    timeout(5 seconds) @@ sequential
}
