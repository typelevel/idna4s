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

sealed abstract class Skew extends Product with Serializable {
  def value: Int

  final override def toString: String = s"Skew(value = ${value})"
}

object Skew {
  final private[this] case class SkewImpl(override val value: Int) extends Skew

  val PunycodeSkew: Skew = unsafeFromInt(38)

  def fromInt(value: Int): Either[String, Skew] =
    if (value >= 1) {
      Right(SkewImpl(value))
    } else {
      Left(s"According to RFC-3492 skew values must be >= 1.")
    }

  def unsafeFromInt(value: Int): Skew =
    fromInt(value).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )

  def fromString(value: String): Either[String, Skew] =
    ApplicativeError[Either[Throwable, *], Throwable]
      .catchNonFatal(
        value.toInt
      )
      .leftMap(_.getLocalizedMessage)
      .flatMap(fromInt)

  def unsafeFromString(value: String): Skew =
    fromString(value).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )
}
