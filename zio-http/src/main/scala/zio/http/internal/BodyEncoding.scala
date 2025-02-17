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

package zio.http.internal

import java.nio.charset.Charset

import zio.http.Body
import zio.http.model.HTTP_CHARSET
import zio.http.netty.NettyBody

trait BodyEncoding {
  def fromCharSequence(charSequence: CharSequence, charset: Charset = HTTP_CHARSET): Body
}

object BodyEncoding {
  val default: BodyEncoding = NettyBody
}
