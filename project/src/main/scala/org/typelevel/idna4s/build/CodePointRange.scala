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
 * A type representing an inclusive Range of code points. This is similar to `Range`, but is
 * used instead because `Range` has unusual equality and ordering properties and by assuming we
 * have a non-empty, inclusive, range it was much easier to write an `Order` instance.
 */
sealed abstract private[build] class CodePointRange extends Serializable {
  def lower: CodePoint
  def upper: CodePoint

  final def size: Int = upper.value - lower.value + 1

  final override def toString: String =
    if (size === 1) {
      s"CodePointRange($lower)"
    } else {
      s"CodePointRange($lower, $upper)"
    }
}

private[build] object CodePointRange {
  final private[this] case class CodePointRangeImpl(
      override val lower: CodePoint,
      override val upper: CodePoint)
      extends CodePointRange

  final case class Single(value: CodePoint) extends CodePointRange {
    override def lower: CodePoint = value
    override def upper: CodePoint = value
  }

  object Single {
    implicit def hashAndOrderForSingle: Hash[Single] with Order[Single] =
      new Hash[Single] with Order[Single] {
        override def hash(x: Single): Int =
          hashAndOrderForCodePointRange.hash(x)

        override def compare(x: Single, y: Single): Int =
          hashAndOrderForCodePointRange.compare(x, y)
      }

    implicit def orderingInstance: Ordering[Single] =
      hashAndOrderForSingle.toOrdering
  }

  def apply(value: CodePoint): CodePointRange =
    Single(value)

  def from(lower: CodePoint, upper: CodePoint): Either[String, CodePointRange] =
    if (lower === upper) {
      Right(apply(lower))
    } else if (lower <= upper) {
      Right(CodePointRangeImpl(lower, upper))
    } else {
      Left(s"Invalid CodePointRange, lower must be <= upper: [$lower, $upper]")
    }

  def unsafeFrom(lower: CodePoint, upper: CodePoint): CodePointRange =
    from(lower, upper).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )

  def fromInt(value: Int): Either[String, CodePointRange] =
    CodePoint.fromInt(value).map(apply)

  def fromInts(lower: Int, upper: Int): Either[String, CodePointRange] =
    for {
      lower <- CodePoint.fromInt(lower)
      upper <- CodePoint.fromInt(upper)
      result <- from(lower, upper)
    } yield result

  def unsafeFromInts(lower: Int, upper: Int): CodePointRange =
    fromInts(lower, upper).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )

  def fromHexString(value: String): Either[String, CodePointRange] =
    CodePoint.fromHexString(value).map(apply)

  def fromHexStrings(lower: String, upper: String): Either[String, CodePointRange] =
    for {
      lower <- CodePoint.fromHexString(lower)
      upper <- CodePoint.fromHexString(upper)
      result <- from(lower, upper)
    } yield result

  implicit val hashAndOrderForCodePointRange: Order[CodePointRange] with Hash[CodePointRange] =
    new Order[CodePointRange] with Hash[CodePointRange] {
      override def hash(x: CodePointRange): Int = x.hashCode

      override def compare(x: CodePointRange, y: CodePointRange): Int =
        (x.lower, x.upper).compare((y.lower, y.upper))
    }

  implicit def orderingInstance: Ordering[CodePointRange] =
    hashAndOrderForCodePointRange.toOrdering

  implicit val showForCodePointRange: Show[CodePointRange] =
    Show.fromToString
}
