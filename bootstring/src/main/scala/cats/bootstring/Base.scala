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

/**
 * A representation of a numerical base, mapping between Unicode code points in the base and
 * numerical values.
 *
 * @note
 *   While this ''technically'' is a general purpose representation, it has a very minimal
 *   feature set.
 *
 * @note
 *   Instances of this type used by RFC-3492 may have unexpected properties. For example, the
 *   base used by [[Base#PunycodeBase]] uses 'A' to represent 0 and '0' represents 26, e.g. 'z'
 *   + 1.
 */
sealed abstract class Base extends Serializable {

  /**
   * A number of digits in the given base
   */
  def value: Int

  /**
   * Attempt to convert a numerical value to the Unicode code point representing a digit with
   * that value in this base.
   */
  def intToCodePointDigit(digit: Int, uppercase: Boolean = false): Either[String, Int]

  /**
   * Attempt to convert a Unicode code point representing a digit in this base to a numerical
   * value.
   */
  def codePointDigitToInt(codePoint: Int): Either[String, Int]

  /**
   * As [[#intToCodePointDigit]], but throws on invalid input.
   */
  def unsafeIntToCodePointDigit(digit: Int, uppercase: Boolean = false): Int

  /**
   * As [[#codePointDigitToInt]], but throws on invalid input.
   */
  def unsafeCodePointDigitToInt(codePoint: Int): Int

  final override def toString: String = s"Base(value = ${value})"
}

object Base {

  /**
   * The base used by the Bootstring algorithm for the Punycode parameters.
   *
   * @note
   *   This base may be surprising as unlike other base encodings (such as hexidecimal), 'A'
   *   represents 0 and '0' represents 26, e.g. 'z' + 1.
   */
  val PunycodeBase: Base = {
    val lowercaseChars: List[Char] =
      (Range.inclusive('a', 'z') ++ Range.inclusive('0', '9')).toList.map(_.toChar)
    val lowercaseArray: Array[Int] =
      lowercaseChars.map(_.toInt).toArray
    val uppercaseArray: Array[Int] =
      lowercaseChars.map(_.toUpper.toInt).toArray

    def codePointDigitToInt(codePoint: Int): Int =
      if (codePoint >= 'A'.toInt && codePoint <= 'Z'.toInt) {
        // A-Z
        codePoint - 'A'.toInt
      } else if (codePoint >= 'a'.toInt && codePoint <= 'z'.toInt) {
        // a-z
        codePoint - 'a'.toInt
      } else if (codePoint >= '0'.toInt && codePoint <= '9'.toInt) {
        // 0-9
        codePoint - 22
      } else {
        throw new IllegalArgumentException(
          s"Code point $codePoint is not valid for the given base.")
      }

    unsafeFrom(36, d => lowercaseArray(d), d => uppercaseArray(d), codePointDigitToInt)
  }

  /**
   * Create a [[Base]] value.
   *
   * @param baseValue
   *   The number of digits in the base.
   * @param intToLowerCodePointF
   *   A function to convert an integer value to a the Unicode code point for the digit with the
   *   corresponding value in this base with a lower case representation. This may throw on
   *   invalid input.
   * @param intToUpperCodePointF
   *   A function to convert an integer value to a the Unicode code point for the digit with the
   *   corresponding value in this base with an upper case representation. This may throw on
   *   invalid input.
   * @param codePointDigitToIntF
   *   A function to convert a Unicode code point representing a digit in this base to the
   *   corresponding numerical value. This may throw on invalid input.
   */
  def unsafeFrom(
      baseValue: Int,
      intToLowerCodePointF: Int => Int,
      intToUpperCodePointF: Int => Int,
      codePointDigitToIntF: Int => Int
  ): Base =
    new Base {
      override val value: Int = baseValue

      final override def unsafeIntToCodePointDigit(digit: Int, uppercase: Boolean): Int =
        if (uppercase) {
          intToUpperCodePointF(digit)
        } else {
          intToLowerCodePointF(digit)
        }

      final override def unsafeCodePointDigitToInt(codePoint: Int): Int =
        codePointDigitToIntF(codePoint)

      final override def intToCodePointDigit(
          digit: Int,
          uppercase: Boolean): Either[String, Int] =
        ApplicativeError[Either[Throwable, *], Throwable]
          .catchNonFatal(
            unsafeIntToCodePointDigit(digit, uppercase)
          )
          .leftMap(_ => s"Digit $digit is not valid for the given base.")

      final override def codePointDigitToInt(codePoint: Int): Either[String, Int] =
        ApplicativeError[Either[Throwable, *], Throwable]
          .catchNonFatal(
            unsafeCodePointDigitToInt(codePoint)
          )
          .leftMap(_.getLocalizedMessage)
    }
}
