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

import java.nio.charset.{Charset, StandardCharsets}

import zio.stacktracer.TracingImplicits.disableAutoTrace

import zio.http.model.headers._
import zio.http.netty.model.headers.{NettyHeaderNames, NettyHeaderValues}

package object model {
  type Header = Headers.Header
  val Header: Headers.Header.type = Headers.Header

  /**
   * Default HTTP Charset
   */
  val HTTP_CHARSET: Charset = StandardCharsets.UTF_8

  object HeaderNames extends HeaderNames with NettyHeaderNames

  object HeaderValues extends HeaderValues with NettyHeaderValues
}
