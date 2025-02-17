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

import scala.annotation.tailrec

import zio.Chunk

/**
 * The Allow header must be sent if the server responds with a 405 Method Not
 * Allowed status code to indicate which request methods can be used.
 */
sealed trait Allow {
  val raw: String
}

object Allow {

  case object OPTIONS                                  extends Allow {
    override val raw: String = "OPTIONS"
  }
  case object GET                                      extends Allow {
    override val raw: String = "GET"
  }
  case object HEAD                                     extends Allow {
    override val raw: String = "HEAD"
  }
  case object POST                                     extends Allow {
    override val raw: String = "POST"
  }
  case object PUT                                      extends Allow {
    override val raw: String = "PUT"
  }
  case object PATCH                                    extends Allow {
    override val raw: String = "PATCH"
  }
  case object DELETE                                   extends Allow {
    override val raw: String = "DELETE"
  }
  case object TRACE                                    extends Allow {
    override val raw: String = "TRACE"
  }
  case object CONNECT                                  extends Allow {
    override val raw: String = "CONNECT"
  }
  case object InvalidAllowMethod                       extends Allow {
    override val raw: String = ""
  }
  final case class AllowMethods(methods: Chunk[Allow]) extends Allow {
    override val raw: String = methods.map(_.raw).mkString(", ")
  }

  private def parseAllowMethod(value: String): Allow =
    value.toUpperCase match {
      case POST.raw    => POST
      case GET.raw     => GET
      case OPTIONS.raw => OPTIONS
      case HEAD.raw    => HEAD
      case PUT.raw     => PUT
      case PATCH.raw   => PATCH
      case DELETE.raw  => DELETE
      case TRACE.raw   => TRACE
      case CONNECT.raw => CONNECT
      case _           => InvalidAllowMethod
    }

  def toAllow(value: String): Allow = {
    @tailrec def loop(index: Int, value: String, acc: AllowMethods): Allow = {
      if (value.isEmpty) AllowMethods(Chunk.empty)
      else if (index == -1) acc.copy(methods = acc.methods ++ Chunk(parseAllowMethod(value.trim)))
      else {
        val valueChunk     = value.substring(0, index)
        val valueRemaining = value.substring(index + 1)
        val newIndex       = valueRemaining.indexOf(',')
        loop(
          newIndex,
          valueRemaining,
          acc.copy(methods = acc.methods ++ Chunk(parseAllowMethod(valueChunk.trim))),
        )
      }
    }

    loop(value.indexOf(','), value, AllowMethods(Chunk.empty))
  }

  def fromAllow(allow: Allow): String = allow match {
    case AllowMethods(methods) => methods.map(fromAllow).filter(_.nonEmpty).mkString(", ")
    case method                => method.raw
  }

}
