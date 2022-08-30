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

import munit._
import scala.annotation.tailrec

final class BaseTests extends ScalaCheckSuite {

  def maxValueConsistentWithBaseValue(base: Base): Unit = {

    @tailrec
    def loop(acc: Int): Int =
      base.intToCodePointDigit(acc) match {
        case Right(_) =>
          loop(acc + 1)
        case Left(_) =>
          acc - 1
      }

    val max: Int = loop(0)

    assertEquals(max, base.value - 1)
  }

  def intToDigitToIntRoundTrip(base: Base): Unit = {

    @tailrec
    def loop(acc: Int): Unit =
      if (acc >= base.value) {
        ()
      } else {
        assertEquals(
          base.intToCodePointDigit(acc).flatMap(base.codePointDigitToInt),
          Right(acc))
        assertEquals(
          base.intToCodePointDigit(acc, true).flatMap(base.codePointDigitToInt),
          Right(acc))
        loop(acc + 1)
      }

    loop(0)
  }

  test("Punycode max numerical value is consistent with intToCodePointDigit.") {
    maxValueConsistentWithBaseValue(Base.PunycodeBase)
  }

  test("Punycode int to code point digit round trip") {
    intToDigitToIntRoundTrip(Base.PunycodeBase)
  }

  test("Punycode code point digit to int round trip") {
    val baseValue: Int =
      (Range('a', 'z').inclusive ++ Range('0', '9').inclusive).toList.foldLeft(0) {
        case (expected, digit) =>
          assertEquals(
            Base.PunycodeBase.codePointDigitToInt(digit.toChar.toLower.toInt),
            Right(expected))
          assertEquals(
            Base.PunycodeBase.codePointDigitToInt(digit.toChar.toUpper.toInt),
            Right(expected))
          expected + 1
      }

    assertEquals(baseValue, 36)
    assertEquals(baseValue, Base.PunycodeBase.value)
  }

  test(
    "Attempting to convert invalid int values should fail gracefully for the safe methods.") {
    assert(Base.PunycodeBase.intToCodePointDigit(-1).isLeft)
    assert(Base.PunycodeBase.intToCodePointDigit(Base.PunycodeBase.value).isLeft)
  }
}
