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

import scala.util.Try

import zio.Chunk

import zio.http.model.MediaType

/** Accept header value. */
sealed trait Accept

object Accept {

  /**
   * The Accept header value one or more MIME types optionally weighed with
   * quality factor.
   */
  final case class AcceptValue(mimeTypes: Chunk[MediaTypeWithQFactor]) extends Accept

  final case class MediaTypeWithQFactor(mediaType: MediaType, qFactor: Option[Double])

  /** The Accept header value is invalid. */
  case object InvalidAcceptValue extends Accept

  def fromAccept(header: Accept): String = header match {
    case AcceptValue(mimeTypes) =>
      mimeTypes.map { case MediaTypeWithQFactor(mime, maybeQFactor) =>
        s"${mime.fullType}${maybeQFactor.map(qFactor => s";q=$qFactor").getOrElse("")}"
      }.mkString(", ")
    case InvalidAcceptValue     => ""
  }

  def toAccept(value: String): Accept = {
    val acceptHeaderValues: Array[MediaTypeWithQFactor] = value
      .split(',')
      .map(_.trim)
      .map { subValue =>
        MediaType
          .forContentType(subValue)
          .map(mt => MediaTypeWithQFactor(mt, extractQFactor(mt)))
          .getOrElse {
            MediaType
              .parseCustomMediaType(subValue)
              .map(mt => MediaTypeWithQFactor(mt, extractQFactor(mt)))
              .orNull
          }
      }

    if (acceptHeaderValues.nonEmpty && acceptHeaderValues.length == acceptHeaderValues.count(_ != null))
      AcceptValue(Chunk.fromArray(acceptHeaderValues))
    else InvalidAcceptValue
  }

  private def extractQFactor(mediaType: MediaType): Option[Double] =
    mediaType.parameters.get("q").flatMap(qFactor => Try(qFactor.toDouble).toOption)
}
