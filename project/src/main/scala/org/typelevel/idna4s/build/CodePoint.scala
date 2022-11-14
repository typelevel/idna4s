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

package org.typelevel.idna4s.build

import cats._
import cats.syntax.all._

/**
 * Newtype for a Unicode code point.
 */
final private[build] class CodePoint private (val value: Int) extends AnyVal {
  final override def toString: String = s"CodePoint(value = ${value})"
}

private[build] object CodePoint {
  private def apply(value: Int): CodePoint =
    new CodePoint(value)

  def unsafeFromInt(value: Int): CodePoint =
    fromInt(value).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )

  def fromInt(value: Int): Either[String, CodePoint] =
    if (value < 0 || value > Character.MAX_CODE_POINT) {
      Left(s"Invalid code point: ${value}")
    } else {
      Right(apply(value))
    }

  def fromHexString(value: String): Either[String, CodePoint] = {
    val trimmed: String = value.trim.toLowerCase
    val integralValue: Either[Throwable, Int] =
      if (trimmed.startsWith("0x")) {
        Either.catchNonFatal(Integer.parseInt(trimmed.drop(2), 16))
      } else {
        Either.catchNonFatal(Integer.parseInt(trimmed, 16))
      }
    integralValue.leftMap(_.getLocalizedMessage).flatMap(fromInt)
  }

  def unsafeFromHexString(value: String): CodePoint =
    fromHexString(value).fold(e => throw new IllegalArgumentException(e), identity)

  implicit val orderInstance: Order[CodePoint] =
    Order.by(_.value)

  implicit def orderingInstance: Ordering[CodePoint] =
    orderInstance.toOrdering
}
