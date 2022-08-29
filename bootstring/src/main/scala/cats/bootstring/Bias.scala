/*
 * Copyright (c) 2022 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package org.typelevel.idna4s.bootstring

import cats._
import cats.syntax.all._

sealed abstract class Bias extends Product with Serializable {
  def value: Int

  final override def toString: String = s"Bias(value = ${value})"
}

object Bias {
  final private[this] case class BiasImpl(override val value: Int) extends Bias

  val PunycodeInitialBias: Bias = unsafeFromInt(72)

  def unsafeFromInt(value: Int): Bias =
    if (value >= 0) {
      BiasImpl(value)
    } else {
      throw new IllegalArgumentException(s"Initial Bias may not be < 0.")
    }

  def fromInt(value: Int): Either[String, Bias] =
    ApplicativeError[Either[Throwable, *], Throwable]
      .catchNonFatal(
        unsafeFromInt(value)
      )
      .leftMap(_.getLocalizedMessage)

  def fromString(value: String): Either[String, Bias] =
    ApplicativeError[Either[Throwable, *], Throwable]
      .catchNonFatal(
        value.toInt
      )
      .leftMap(_.getLocalizedMessage)
      .flatMap(fromInt)

  def unsafeFromString(value: String): Bias =
    fromString(value).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )
}
