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

package org.typelevel.idna4s.tests

import cats.kernel.laws.discipline._
import cats.syntax.all._
import munit._
import org.scalacheck.Prop._
import org.scalacheck._
import org.typelevel.idna4s.core._
import org.typelevel.idna4s.core.syntax.all._
import org.typelevel.idna4s.scalacheck.all._
import scala.annotation.tailrec

final class CodePointTests extends DisciplineSuite {

  test("CodePoint.fromInt should succeed for all code points") {

    @tailrec
    def loop(i: Int): Unit =
      if (i > Character.MAX_CODE_POINT) {
        ()
      } else {
        assert(CodePoint.fromInt(i).isRight)
        loop(i + 1)
      }

    loop(0)
  }

  test("CodePoint round trip") {

    @tailrec
    def loop(i: Int): Unit =
      if (i > Character.MAX_CODE_POINT) {
        ()
      } else {
        val cp: CodePoint = CodePoint.unsafeFromInt(i)
        assertEquals(cp.value, i)

        cp.asCharList match {
          case x :: Nil =>
            assertEquals(CodePoint.unsafeFromInt(x.toInt), cp)
          case x :: y :: Nil =>
            assertEquals(CodePoint.unsafeFromInt(Character.toCodePoint(x, y)), cp)
          case _ =>
            fail(s"CodePoint ${i} did not yield 1 or 2 chars.")
        }

        loop(i + 1)
      }

    loop(0)
  }

  test("CodePoint.fromChar should succeed for all chars") {

    @tailrec
    def loop(i: Int): Unit =
      if (i > Char.MaxValue.toInt) {
        ()
      } else {
        assertEquals(CodePoint.fromChar(i.toChar).value, i)

        loop(i + 1)
      }

    loop(0)

  }

  test("CodePoint.fromString should parse valid code points") {

    @tailrec
    def loop(i: Int): Unit =
      if (i > Character.MAX_CODE_POINT) {
        ()
      } else {
        assert(CodePoint.fromString(i.toString).isRight)
        assert(CodePoint.fromString(String.format("0x%04X", Integer.valueOf(i))).isRight)
        assert(CodePoint.fromString(String.format("0X%04X", Integer.valueOf(i))).isRight)

        loop(i + 1)
      }

    loop(0)
  }

  test("CodePoint.fromString fails for non integral strings") {
    assert(CodePoint.fromString("DERP").isLeft)
  }

  property("CodePoint.fromInt should fail for non code points")(
    forAll(genNonCodePoint)(i => Prop(CodePoint.fromInt(i).isLeft) :| "CodePoint.fromInt fails")
  )

  test("Literal syntax for valid code points should compile") {
    assertEquals(codePoint"0", CodePoint.unsafeFromInt(0))
    assertEquals(codePoint"0x0", CodePoint.unsafeFromInt(0))
    assertEquals(codePoint"0X0", CodePoint.unsafeFromInt(0))
  }

  test("Literal syntax for invalid literals should not compile") {
    compileErrors("""codePoint"DERP"""").contains(
      "Given value is not a valid non-negative integral value")
    compileErrors("""codePoint"F"""").contains(
      "Given value is not a valid non-negative integral value")
  }

  test("CodePoint.toString should not throw any NPEs") {
    @tailrec
    def loop(i: Int): Unit =
      if (i > Character.MAX_CODE_POINT) {
        ()
      } else {
        assert(CodePoint.unsafeFromInt(i).toString.size > 0)

        loop(i + 1)
      }

    loop(0)
  }

  test("CodePoint.isHighSurrogate aggress with java.lang.Character") {
    @tailrec
    def loop(i: Int): Unit =
      if (i > Character.MAX_HIGH_SURROGATE) {
        ()
      } else {
        assertEquals(
          CodePoint.unsafeFromInt(i).isHighSurrogate,
          Character.isHighSurrogate(i.toChar))

        loop(i + 1)
      }

    loop(Character.MIN_HIGH_SURROGATE)
  }

  test("CodePoint.isLowSurrogate aggress with java.lang.Character") {
    @tailrec
    def loop(i: Int): Unit =
      if (i > Character.MAX_LOW_SURROGATE) {
        ()
      } else {
        assertEquals(
          CodePoint.unsafeFromInt(i).isLowSurrogate,
          Character.isLowSurrogate(i.toChar))

        loop(i + 1)
      }

    loop(Character.MIN_LOW_SURROGATE)
  }

  test("CodePoint.isSurrogate aggress with java.lang.Character") {
    @tailrec
    def loop(i: Int): Unit =
      if (i > Character.MAX_SURROGATE) {
        ()
      } else {
        assertEquals(CodePoint.unsafeFromInt(i).isSurrogate, Character.isSurrogate(i.toChar))

        loop(i + 1)
      }

    loop(Character.MIN_SURROGATE)
  }

  test("CodePoint's information methods should be consistent with each other.") {

    @tailrec
    def loop(i: Int): Unit =
      if (i > Character.MAX_CODE_POINT) {
        ()
      } else {
        val cp: CodePoint = CodePoint.unsafeFromInt(i)

        cp.utf16CharCount match {
          case 1 =>
            assert(cp.asChars.isLeft)
            assertEquals(cp.asCharList.size, 1)
            assertEquals(cp.asCharList.headOption, cp.asChars.swap.toOption)
          case 2 =>
            assert(cp.asChars.isRight)
            assertEquals(cp.asCharList.size, 2)
            assertEquals(
              cp.asCharList,
              cp.asChars.map { case (a, b) => List(a, b) }.toOption.toList.flatten)
            assertEquals(cp.isSurrogate, false)
            assertEquals(cp.isLowSurrogate, false)
            assertEquals(cp.isHighSurrogate, false)
          case otherwise =>
            fail(s"Impossible utf16CharCount: ${otherwise}")
        }

        assert((cp.isSurrogate === false) || (cp.isLowSurrogate ^ cp.isHighSurrogate))

        loop(i + 1)
      }

    loop(0)
  }

  // Laws //

  checkAll("Order[CodePoint]", OrderTests[CodePoint].order)
  checkAll("Hash[CodePoint]", HashTests[CodePoint].hash)
  checkAll("LowerBounded[CodePoint]", LowerBoundedTests[CodePoint].lowerBounded)
  checkAll("UpperBounded[CodePoint]", UpperBoundedTests[CodePoint].upperBounded)
}
