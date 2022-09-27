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

package org.typelevel.idna4s.core

import cats.kernel._
import cats.Show
import cats.syntax.all._

/**
 * A newtype for representing a Unicode code point.
 *
 * Unicode code points are a integral values from [0, 0x10FFFF].
 *
 * This type is not (and should not) generally be used in critical path performance sensitive
 * code. It is mostly useful for generating error information, testing, and for some performance
 * insensitive methods which operate only a single code point. In particular for error messages
 * where we generally should not render arbitrary code points as they may influence the output
 * text in ways we do not anticipate, such as bidi scripts or where the code point in isolation
 * is not able to be rendered at all.
 *
 * The `toString` for this class does ''not'' render the `String` representation of this code
 * point, it renders the base 10 and base 16 value of the code point, as well as the number of
 * UTF-16 char values needed to represent this code point and if possible a prose description of
 * the code point. The reason for not rendering the `String` representation is that this type is
 * often used in error cases where the code point will have no valid `String` representation, or
 * where that representation is not meaningful, or where that representation could mangle the
 * error message (as noted above).
 *
 * For example,
 *
 * {{{
 * scala> CodePoint.fromChar('a')
 * val res0: Either[String, org.typelevel.idna4s.core.CodePoint] = Right(CodePoint(value = 97, hexValue = 0x0061, name = LATIN SMALL LETTER A, utf16CharCount = 1))
 * }}}
 *
 * @see
 *   [[https://www.unicode.org/reports/tr36/ Unicode Security Considerations]]
 */
final class CodePoint private (val value: Int) extends AnyVal {

  /**
   * A given char value in UTF-16 code point may be represented by 1-2 char values.
   */
  def utf16CharCount: Int =
    if (value >= 0x10000) 2 else 1

  /**
   * Convert this code point to a list of UTF-16 char values, either size 1 or 2 depending on if
   * this code point is represents a surrogate pair in UTF-16.
   */
  def toChars: List[Char] =
    Character.toChars(value).toList

  /**
   * True if the code point represents a high surrogate code point.
   *
   * A high surrogate code point is a code point that when followed by a valid low surrogate
   * code point in a UTF-16 char sequence, represents a different code point.
   *
   * For example,
   *
   * {{{
   * scala> val high = codePoint"0xd800"
   * val high: org.typelevel.idna4s.core.CodePoint = CodePoint(value = 55296, hexValue = 0xD800, name = HIGH SURROGATES D800, utf16CharCount = 1)
   *
   * scala> val low = codePoint"0xdc00"
   * val low: org.typelevel.idna4s.core.CodePoint = CodePoint(value = 56320, hexValue = 0xDC00, name = LOW SURROGATES DC00, utf16CharCount = 1)
   *
   * scala> val combined = CodePoint.unsafeFromInt(Character.toCodePoint(high.toChars.head, low.toChars.head))
   * val combined: org.typelevel.idna4s.core.CodePoint = CodePoint(value = 65536, hexValue = 0x10000, name = LINEAR B SYLLABLE B008 A, utf16CharCount = 2)
   *
   * scala> high == CodePoint.fromChar(combined.toChars.head)
   * val res0: Boolean = true
   *
   * scala> low == CodePoint.fromChar(combined.toChars.tail.head)
   * val res1: Boolean = true
   * }}}
   *
   * Care must be taken when working with these code points. Usually, when thinking in terms of
   * Unicode characters, one does not want to directly operation on partial surrogate values. If
   * you pull one of these values directly out of a `String` and you intend to be operating on
   * code points or Unicode characters, that likely means you have either indexed the `String`
   * incorrectly, e.g. by char values not by code points or that the `String` not valid Unicode
   * malformed. The `String` type does not validated that the component char values make up a
   * valid Unicode character sequence, only that they are all valid code points.
   *
   * {{{
   * // This does not have, and will never have,
   * // a valid Unicode representation, because it is two high (leading)
   * // chars in a row.
   * scala> val invalid = new String(Array(0xd800.toChar, 0xd800.toChar))
   * val invalid: String = ??
   * }}}
   *
   * @note
   *   These code points are also sometimes referred to as "leading" surrogate code points.
   */
  def isHighSurrogate: Boolean =
    value >= 0xd800 && value <= 0xdbff

  /**
   * True if the code point represents a low surrogate code point.
   *
   * A low surrogate code point is a code point that when following a valid high surrogate code
   * point in a UTF-16 char sequence, represents a different code point.
   *
   * For example,
   *
   * {{{
   * scala> val high = codePoint"0xd800"
   * val high: org.typelevel.idna4s.core.CodePoint = CodePoint(value = 55296, hexValue = 0xD800, name = HIGH SURROGATES D800, utf16CharCount = 1)
   *
   * scala> val low = codePoint"0xdc00"
   * val low: org.typelevel.idna4s.core.CodePoint = CodePoint(value = 56320, hexValue = 0xDC00, name = LOW SURROGATES DC00, utf16CharCount = 1)
   *
   * scala> val combined = CodePoint.unsafeFromInt(Character.toCodePoint(high.toChars.head, low.toChars.head))
   * val combined: org.typelevel.idna4s.core.CodePoint = CodePoint(value = 65536, hexValue = 0x10000, name = LINEAR B SYLLABLE B008 A, utf16CharCount = 2)
   *
   * scala> high == CodePoint.fromChar(combined.toChars.head)
   * val res0: Boolean = true
   *
   * scala> low == CodePoint.fromChar(combined.toChars.tail.head)
   * val res1: Boolean = true
   * }}}
   *
   * Care must be taken when working with these code points. Usually, when thinking in terms of
   * Unicode characters, one does not want to directly operation on partial surrogate values. If
   * you pull one of these values directly out of a `String` and you intend to be operating on
   * code points or Unicode characters, that likely means you have either indexed the `String`
   * incorrectly, e.g. by char values not by code points or that the `String` not valid Unicode
   * malformed. The `String` type does not validated that the component char values make up a
   * valid Unicode character sequence, only that they are all valid code points.
   *
   * {{{
   * // This does not have, and will never have,
   * // a valid Unicode representation, because it is two high (leading)
   * // chars in a row.
   * scala> val invalid = new String(Array(0xd800.toChar, 0xd800.toChar))
   * val invalid: String = ??
   * }}}
   *
   * @note
   *   These code points are also sometimes referred to as "leading" surrogate code points.
   */
  def isLowSurrogate: Boolean =
    value >= 0xdc00 && value <= 0xdfff

  /**
   * True if the code point is either a high or low surrogate code point.
   *
   * See [[#isLowSurrogate]] and [[#isHighSurrogate]] for more information.
   */
  def isSurrogate: Boolean =
    isHighSurrogate || isLowSurrogate

  override def toString: String =
    CodePoint
      .nameForCodePoint(this)
      .fold(
        s"CodePoint(value = ${value}, hexValue = ${CodePoint.intToHex(
            value)}, utf16CharCount = ${utf16CharCount}, isLowSurrogate = ${isLowSurrogate}, isHighSurrogate = ${isHighSurrogate})"
      )(name =>
        s"CodePoint(value = ${value}, hexValue = ${CodePoint.intToHex(
            value)}, name = ${name}, utf16CharCount = ${utf16CharCount}, isLowSurrogate = ${isLowSurrogate}, isHighSurrogate = ${isHighSurrogate})")
}

// CodePointPlatform provides a nameForCodePoint method. This uses the
// Character.getName method on the JVM, but that method is not implemented on
// Native and ScalaJS. It's _really_ nice to have, so I added platform code
// for it so we can at least have it on the JVM.

object CodePoint extends CodePointPlatform {

  private def intToHex(value: Int): String =
    String.format("0x%04X", Integer.valueOf(value))

  val MinValue: CodePoint = CodePoint(0)
  val MaxValue: CodePoint = CodePoint(Character.MAX_CODE_POINT)

  def unapply(value: CodePoint): Some[Int] =
    Some(value.value)

  private def apply(value: Int): CodePoint =
    new CodePoint(value)

  /**
   * Attempt to create a [[CodePoint]] from an int, throwing an exception if the int is not a
   * valid Unicode code point.
   */
  def unsafeFromInt(value: Int): CodePoint =
    if (value < 0 || value > Character.MAX_CODE_POINT) {
      throw new IllegalArgumentException(
        s"Given integral value is not a valid Unicode code point: ${intToHex(value)}")
    } else {
      CodePoint(value)
    }

  /**
   * Attempt to create a [[CodePoint]] from an int, yielding an error if the int is not a valid
   * Unicode code point.
   */
  def fromInt(value: Int): Either[String, CodePoint] =
    Either.catchNonFatal(unsafeFromInt(value)).leftMap(_.getLocalizedMessage)

  /**
   * Create a Unicode code point from a char value.
   *
   * @note
   *   This method will create a code point from any char value, including partial surrogate
   *   values.
   */
  def fromChar(value: Char): CodePoint =
    unsafeFromInt(value.toInt)

  /**
   * Attempt to parse a `String` as a Unicode code point, throwing an exception if the `String`
   * is not a valid Unicode code point.
   *
   * Base 10 and base 16 numbers are valid. Base 16 values must be prefixed with "0x" or "0X".
   */
  def unsafeFromString(value: String): CodePoint =
    if (value.trim.toLowerCase.startsWith("0x")) {
      unsafeFromInt(Integer.parseInt(value.trim.drop(2), 16))
    } else {
      unsafeFromInt(value.toInt)
    }

  /**
   * Attempt to parse a `String` as a Unicode code point, yielding an error if the `String` is
   * not a valid Unicode code point.
   *
   * Base 10 and base 16 numbers are valid. Base 16 values must be prefixed with "0x" or "0X".
   */
  def fromString(value: String): Either[String, CodePoint] =
    Either.catchNonFatal(unsafeFromString(value)).leftMap {
      case _: NumberFormatException =>
        "Given value is not a valid non-negative integral value"
      case e =>
        e.getLocalizedMessage
    }

  implicit val hashAndOrderForCodePoint: Hash[CodePoint] with Order[CodePoint] =
    new Hash[CodePoint] with Order[CodePoint] {
      override def hash(x: CodePoint): Int = x.hashCode

      override def compare(x: CodePoint, y: CodePoint): Int =
        x.value.compare(y.value)
    }

  implicit def ordering: Ordering[CodePoint] =
    hashAndOrderForCodePoint.toOrdering

  implicit val showForCodePoint: Show[CodePoint] =
    Show.fromToString

  implicit val lowerBoundForCodePoint: LowerBounded[CodePoint] =
    new LowerBounded[CodePoint] {
      override def partialOrder: PartialOrder[CodePoint] = hashAndOrderForCodePoint

      override def minBound: CodePoint = CodePoint.MinValue
    }

  implicit val upperBoundForCodePoint: UpperBounded[CodePoint] =
    new UpperBounded[CodePoint] {
      override def partialOrder: PartialOrder[CodePoint] = hashAndOrderForCodePoint

      override def maxBound: CodePoint = CodePoint.MaxValue
    }
}
