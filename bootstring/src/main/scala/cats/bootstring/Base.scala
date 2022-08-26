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

sealed abstract class Base extends Serializable {
  def value: Int

  def digitToCodePoint(digit: Int, uppercase: Boolean = false): Either[String, Int]

  def codePointToDigit(codePoint: Int): Either[String, Int]

  def unsafeDigitToCodePoint(digit: Int, uppercase: Boolean = false): Int

  def unsafeCodePointToDigit(codePoint: Int): Int

  final override def toString: String = s"Base(value = ${value})"
}

object Base {

  val PunycodeBase: Base = {
    val lowercaseChars: List[Char] =
      (Range.inclusive('a', 'z') ++ Range.inclusive('0', '9')).toList.map(_.toChar)

    def codePointToDigit(codePoint: Int): Int =
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
          s"Code point $codePoint is valid for the given base.")
      }

    unsafeFrom(36, lowercaseChars, codePointToDigit)
  }

  def from(
      baseValue: Int,
      codePointList: List[Char],
      codePointToDigitF: Int => Int
  ): Either[String, Base] =
    if (codePointList.size === baseValue) {
      val codePointArray: Array[Char] = codePointList.toArray
      Right(
        new Base {
          override val value: Int = baseValue

          final override def unsafeDigitToCodePoint(
              digit: Int,
              uppercase: Boolean = false): Int =
            if (uppercase) {
              codePointArray(digit).toUpper.toInt
            } else {
              codePointArray(digit).toLower.toInt
            }

          final override def unsafeCodePointToDigit(codePoint: Int): Int =
            codePointToDigitF(codePoint)

          final override def digitToCodePoint(
              digit: Int,
              uppercase: Boolean = false): Either[String, Int] =
            ApplicativeError[Either[Throwable, *], Throwable]
              .catchNonFatal(
                unsafeDigitToCodePoint(digit, uppercase)
              )
              .leftMap(_.getLocalizedMessage)

          final override def codePointToDigit(codePoint: Int): Either[String, Int] =
            ApplicativeError[Either[Throwable, *], Throwable]
              .catchNonFatal(
                unsafeCodePointToDigit(codePoint)
              )
              .leftMap(_.getLocalizedMessage)
        }
      )
    } else {
      Left(
        s"Base values must be equal to the size of the defined code points for each base digit. codePointList size is ${codePointList.size}, base value is ${baseValue}")
    }

  def unsafeFrom(
      baseValue: Int,
      codePointList: List[Char],
      codePointToDigit: Int => Int
  ): Base =
    from(baseValue, codePointList, codePointToDigit).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )
}
