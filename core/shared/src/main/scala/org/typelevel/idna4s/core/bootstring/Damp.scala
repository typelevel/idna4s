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

package org.typelevel.idna4s.core.bootstring

import cats.Show
import cats.kernel._
import cats.syntax.all._

/**
 * Newtype for RFC 3492 Damp value.
 *
 * A damp is a special value used in the bias adaptation function of Bootstring when scaling the
 * very first delta. It compensates for the fact that the second delta is usually much smaller
 * the first.
 *
 * @see
 *   [[https://datatracker.ietf.org/doc/html/rfc3492#section-3.4 Bias Adaptation]]
 */
final class Damp private (val value: Int) extends AnyVal {
  final override def toString: String = s"Damp(value = ${value})"
}

object Damp {
  private def apply(value: Int): Damp =
    new Damp(value)

  val MinValue: Damp = Damp(2)
  val MaxValue: Damp = Damp(Int.MaxValue)

  /**
   * The damp used by the Punycode Bootstring invocation.
   */
  final val PunycodeDamp: Damp = new Damp(700)

  /**
   * Attempt to create a [[Damp]] value from an int value, throwing an exception if it is
   * outside the valid domain for a [[Damp]] value.
   */
  def unsafeFromInt(value: Int): Damp =
    if (value >= 2) {
      Damp(value)
    } else {
      throw new IllegalArgumentException(
        s"According to RFC-3492 damp values must be >= 2: ${value}")
    }

  /**
   * Attempt to create a [[Damp]] value from a `String` value, representing a base 10 int32
   * value, throwing an exception if the `String` is not a valid base 10 int32 value or if it is
   * outside the valid domain for a [[Damp]] value.
   */
  def unsafeFromString(value: String): Damp =
    unsafeFromInt(value.toInt)

  /**
   * Attempt to create a [[Damp]] value from an int value, yielding an error if it is outside
   * the valid domain for a [[Damp]] value.
   */
  def fromInt(value: Int): Either[String, Damp] =
    Either.catchNonFatal(unsafeFromInt(value)).leftMap(_.getLocalizedMessage)

  /**
   * Attempt to create a [[Damp]] value from a `String` value, representing a base 10 int32
   * value, yielding an error if the `String` is not a valid base 10 int32 value or if it is
   * outside the valid domain for a [[Damp]] value.
   */
  def fromString(value: String): Either[String, Damp] =
    Either.catchNonFatal(unsafeFromString(value.trim)).leftMap(_.getLocalizedMessage)

  implicit val hashAndOrderForDamp: Hash[Damp] with Order[Damp] =
    new Hash[Damp] with Order[Damp] {
      override def hash(x: Damp): Int =
        x.hashCode

      override def compare(x: Damp, y: Damp): Int =
        x.value.compare(y.value)
    }

  implicit def orderingForDamp: Ordering[Damp] =
    hashAndOrderForDamp.toOrdering

  implicit val showForDamp: Show[Damp] =
    Show.fromToString

  implicit val lowerBoundedForDamp: LowerBounded[Damp] =
    new LowerBounded[Damp] {
      override def partialOrder: PartialOrder[Damp] =
        hashAndOrderForDamp

      override def minBound: Damp = MinValue
    }

  implicit val upperBoundedForDamp: UpperBounded[Damp] =
    new UpperBounded[Damp] {
      override def partialOrder: PartialOrder[Damp] =
        hashAndOrderForDamp

      override def maxBound: Damp = MaxValue
    }
}
