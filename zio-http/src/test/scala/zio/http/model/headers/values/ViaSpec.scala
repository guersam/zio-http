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

package zio.http.model.headers.values

import zio.test.{Spec, TestEnvironment, ZIOSpecDefault, assertTrue}
import zio.{Chunk, Scope}

import zio.http.model.headers.values.Via.ReceivedProtocol

object ViaSpec extends ZIOSpecDefault {
  override def spec: Spec[TestEnvironment with Scope, Any] = suite("Via suite")(
    test("parsing of valid values") {
      assertTrue(
        Via.toVia("1.1 vegur") == Via.ViaValues(
          Chunk(Via.DetailedValue(ReceivedProtocol.Version("1.1"), "vegur", None)),
        ),
      ) &&
      assertTrue(
        Via.toVia("HTTP/1.1 GWA") == Via.ViaValues(
          Chunk(Via.DetailedValue(ReceivedProtocol.ProtocolVersion("HTTP", "1.1"), "GWA", None)),
        ),
      ) &&
      assertTrue(
        Via.toVia("1.0 fred, 1.1 p.example.net") == Via.ViaValues(
          Chunk(
            Via.DetailedValue(ReceivedProtocol.Version("1.0"), "fred", None),
            Via.DetailedValue(ReceivedProtocol.Version("1.1"), "p.example.net", None),
          ),
        ),
      )
      assertTrue(
        Via.toVia("1.0 fred, 1.1 p.example.net, 1.1 nowhere.com (Apache/1.1)") == Via.ViaValues(
          Chunk(
            Via.DetailedValue(ReceivedProtocol.Version("1.0"), "fred", None),
            Via.DetailedValue(ReceivedProtocol.Version("1.1"), "p.example.net", None),
            Via.DetailedValue(ReceivedProtocol.Version("1.1"), "nowhere.com", Some("(Apache/1.1)")),
          ),
        ),
      )
    },
  )
}
