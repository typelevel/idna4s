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
