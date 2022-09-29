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
import org.typelevel.idna4s.core._

/**
 * A Unicode code point which acts as a delimiter between the basic code points and the
 * non-basic code points in a Bootstring encoded `String`.
 *
 * From RFC-3492,
 *
 * "All basic code points appearing in the extended string are represented literally at the
 * beginning of the basic string, in their original order, followed by a delimiter if (and only
 * if) the number of basic code points is nonzero. The delimiter is a particular basic code
 * point, which never appears in the remainder of the basic string. The decoder can therefore
 * find the end of the literal portion (if there is one) by scanning for the last delimiter."
 *
 * @note
 *   Technically, the code point can be any Unicode code point. This means that it ''might'' be
 *   represented by more than one char in a UTF-16 `String`.
 *
 * @note
 *   While RFC-3492 seems to imply that any code point can be used as a delimiter, the use of a
 *   partial surrogate as a delimiter creates a situation of possible ambiguous parsing. For
 *   example, if the delimiter is chosen to be a high surrogate value, and the first code point
 *   in the non-basic set is a low surrogate value, this will create a new code point from the
 *   combination of the pair. The result of this would make it impossible to unambiguously
 *   detect the delimiter in the input. For this reason, [[Delimiter]] forbids the use of code
 *   points which are part of a surrogate pair.
 *
 * @note
 *   Because this type forbids code points which are high or low surrogate values, it represents
 *   a non-continuous subset of valid Unicode code points.
 *
 * @see
 *   [[https://datatracker.ietf.org/doc/html/rfc3492#section-3.1 Basic code point segregation]]
 * @see
 *   [[https://www.unicode.org/versions/Unicode15.0.0/ch03.pdf#G2630 Unicode Surrogates]]
 */
final class Delimiter private (val codePointInt: Int) extends AnyVal {

  /**
   * The code point as a [[CodePoint]]. Mostly for informational purposes, such as error
   * messages.
   */
  final def codePoint: CodePoint =
    CodePoint.unsafeFromInt(codePointInt)

  final override def toString: String =
    s"Delimiter(codePoint = ${codePoint})"
}

object Delimiter {

  private def apply(value: Int): Delimiter =
    new Delimiter(value)

  def unapply(value: Delimiter): Some[Int] =
    Some(value.codePointInt)

  /**
   * The [[Delimiter]] used by the Punycode variant of Bootstring, '-'.
   */
  val PunycodeDelimiter: Delimiter = unsafeFromChar('-')

  /**
   * Attempt to create a [[Delimiter]] value from a [[CodePoint]], failing if the given
   * [[CodePoint]] is a high or low surrogate.
   *
   * @see
   *   [[https://www.unicode.org/versions/Unicode15.0.0/ch03.pdf#G2630 Unicode Surrogates]]
   */
  def fromCodePoint(codePoint: CodePoint): Either[String, Delimiter] =
    if (codePoint.isSurrogate === false) {
      Right(Delimiter(codePoint.value))
    } else {
      Left(
        s"Refusing to create RFC-3492 delimiter from code point which is a high or low surrogate value: ${codePoint}")
    }

  /**
   * Attempt to create a [[Delimiter]] value from a [[CodePoint]], throwing an exception if the
   * given [[CodePoint]] is a high or low surrogate.
   *
   * @see
   *   [[https://www.unicode.org/versions/Unicode15.0.0/ch03.pdf#G2630 Unicode Surrogates]]
   */
  def unsafeFromCodePoint(codePoint: CodePoint): Delimiter =
    fromCodePoint(codePoint).fold(e => throw new IllegalArgumentException(e), identity)

  /**
   * Attempt to create a [[Delimiter]] value from an arbitrary int32 value, failing if the value
   * is not a valid Unicode code point or if the code point is a high or low surrogate value.
   *
   * @see
   *   [[https://www.unicode.org/versions/Unicode15.0.0/ch03.pdf#G2630 Unicode Surrogates]]
   */
  def fromInt(codePoint: Int): Either[String, Delimiter] =
    CodePoint.fromInt(codePoint).flatMap(fromCodePoint)

  /**
   * Attempt to create a [[Delimiter]] value from an arbitrary int32 value, throwing an
   * exception if the value is not a valid Unicode code point or if the code point is a high or
   * low surrogate value.
   *
   * @see
   *   [[https://www.unicode.org/versions/Unicode15.0.0/ch03.pdf#G2630 Unicode Surrogates]]
   */
  def unsafeFromInt(codePoint: Int): Delimiter =
    fromInt(codePoint).fold(e => throw new IllegalArgumentException(e), identity)

  /**
   * Attempt to create a [[Delimiter]] from a char value, failing if the char value is a high or
   * low surrogate value.
   *
   * @see
   *   [[https://www.unicode.org/versions/Unicode15.0.0/ch03.pdf#G2630 Unicode Surrogates]]
   */
  def fromChar(char: Char): Either[String, Delimiter] =
    fromCodePoint(CodePoint.fromChar(char))

  /**
   * Attempt to create a [[Delimiter]] from a char value, throwing an exception if the char
   * value is a high or low surrogate value.
   *
   * @see
   *   [[https://www.unicode.org/versions/Unicode15.0.0/ch03.pdf#G2630 Unicode Surrogates]]
   */
  def unsafeFromChar(char: Char): Delimiter =
    fromChar(char).fold(e => throw new IllegalArgumentException(e), identity)

  /**
   * Attempt to create a [[Delimiter]] value from a `String`. This will fail if the `String`
   * contains anything other than a single Unicode code point or if the code point is a high or
   * low surrogate value.
   *
   * @see
   *   [[https://www.unicode.org/versions/Unicode15.0.0/ch03.pdf#G2630 Unicode Surrogates]]
   */
  def fromString(value: String): Either[String, Delimiter] = {
    lazy val error: Either[String, Delimiter] =
      Left(
        s"A bootstring delimiter must be a single code point, the given value is invalid: ${value}")
    if (value.length < 3 && value.length > 0) {
      CodePoint
        .fromInt(value.codePointAt(0))
        .flatMap(cp =>
          if (value.length === 1 || cp.utf16CharCount === 2) {
            // if value.length is 2 and cp.utf16CharCount is 1, then we have 2
            // code points, not 1.
            fromCodePoint(cp)
          } else {
            error
          })
    } else {
      error
    }
  }

  /**
   * Attempt to create a [[Delimiter]] value from a `String`. This will throw an exception if
   * the `String` contains anything other than a single Unicode code point or if the code point
   * is a high or low surrogate value.
   */
  def unsafeFromString(value: String): Delimiter =
    fromString(value).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )

  implicit val hashAndOrderForDelimiter: Hash[Delimiter] with Order[Delimiter] =
    new Hash[Delimiter] with Order[Delimiter] {
      override def hash(x: Delimiter): Int =
        x.hashCode

      override def compare(x: Delimiter, y: Delimiter): Int =
        x.codePointInt.compare(y.codePointInt)
    }

  implicit def orderingInstance: Ordering[Delimiter] =
    hashAndOrderForDelimiter.toOrdering

  implicit val showForDelimiter: Show[Delimiter] =
    Show.fromToString
}
