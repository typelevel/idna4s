/*
 * Copyright 2022 Typelevel
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

package org.typelevel.idna4s.bootstring

import cats._
import cats.syntax.all._

sealed abstract class TMin extends Product with Serializable {
  def value: Int

  final override def toString: String = s"TMin(value = ${value})"
}

object TMin {
  final private[this] case class TMinImpl(override val value: Int) extends TMin

  val PunycodeTMin: TMin = unsafeFromInt(1)

  def fromInt(value: Int): Either[String, TMin] =
    if (value >= 0) {
      Right(TMinImpl(value))
    } else {
      Left(s"According to RFC-3492 damp values must be >= 0.")
    }

  def unsafeFromInt(value: Int): TMin =
    fromInt(value).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )

  def fromString(value: String): Either[String, TMin] =
    ApplicativeError[Either[Throwable, *], Throwable]
      .catchNonFatal(
        value.toInt
      )
      .leftMap(_.getLocalizedMessage)
      .flatMap(fromInt)

  def unsafeFromString(value: String): TMin =
    fromString(value).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )
}
