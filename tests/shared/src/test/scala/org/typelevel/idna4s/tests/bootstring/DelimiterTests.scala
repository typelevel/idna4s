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

package org.typelevel.idna4s.tests.bootstring

import cats.kernel.laws.discipline._
import cats.syntax.all._
import munit._
import org.scalacheck.Prop._
import org.scalacheck._
import org.typelevel.idna4s.core._
import org.typelevel.idna4s.core.bootstring._
import org.typelevel.idna4s.core.syntax.all._
import org.typelevel.idna4s.scalacheck.all._
import scala.annotation.tailrec

final class DelimiterTests extends DisciplineSuite {

  test("Delimiter.fromInt should succeed for all code points, which are not surrogate values") {

    @tailrec
    def loop(i: Int): Unit =
      if (i > Character.MAX_CODE_POINT) {
        ()
      } else {
        val cp: CodePoint = CodePoint.unsafeFromInt(i)

        if (cp.isSurrogate) {
          assert(Delimiter.fromInt(i).isLeft)
          assert(Delimiter.fromCodePoint(cp).isLeft)
        } else {
          assert(Delimiter.fromInt(i).isRight)
          assert(Delimiter.fromCodePoint(cp).isRight)
        }

        loop(i + 1)
      }

    loop(0)
  }

  test("Delimiter round trip") {

    @tailrec
    def loop(i: Int): Unit =
      if (i > Character.MAX_CODE_POINT) {
        ()
      } else if (i === Character.MIN_SURROGATE) {
        loop(Character.MAX_SURROGATE + 1)
      } else {
        val d: Delimiter = Delimiter.unsafeFromInt(i)
        assertEquals(d.codePointInt, i)

        d.codePoint.asChars match {
          case Left(x) =>
            assertEquals(Delimiter.unsafeFromChar(x).codePoint, d.codePoint)
          case Right((x, y)) =>
            assertEquals(Delimiter.unsafeFromInt(Character.toCodePoint(x, y)), d)
        }

        loop(i + 1)
      }

    loop(0)
  }

  test(
    "Delimiter construction for chars should succeed for all chars which are not surrogates") {

    @tailrec
    def loop(i: Int): Unit =
      if (i >= Char.MaxValue.toInt) {
        ()
      } else {
        val cp: CodePoint = CodePoint.fromChar(i.toChar)
        if (cp.isSurrogate) {
          assert(Delimiter.fromChar(i.toChar).isLeft)
        } else {
          assert(Delimiter.fromChar(i.toChar).isRight)
        }

        loop(i + 1)
      }

    loop(0)
  }

  test("Delimiter.fromString should parse valid code points which are not surrogates") {

    @tailrec
    def loop(i: Int): Unit =
      if (i > Character.MAX_CODE_POINT) {
        ()
      } else if (i === Character.MIN_SURROGATE) {
        loop(Character.MAX_SURROGATE + 1)
      } else {
        assertEquals(
          Delimiter.fromString(new String(Character.toChars(i))),
          Right(Delimiter.unsafeFromInt(i)))

        loop(i + 1)
      }

    loop(0)
  }

  test("Delimiter.fromString fails for non integral strings") {
    assert(CodePoint.fromString("DERP").isLeft)
  }

  property("Delimiter.fromInt should fail for non code points")(
    forAll(genNonCodePoint)(i => Prop(Delimiter.fromInt(i).isLeft) :| "Delimiter.fromInt fails")
  )

  test("Literal syntax for valid Delimiter should compile") {
    assertEquals(delimiter"0", Delimiter.unsafeFromChar('0'))
    assertEquals(delimiter"-", Delimiter.unsafeFromChar('-'))

    // Multi char code point
    assertEquals(delimiter"𐀀", Delimiter.unsafeFromCodePoint(codePoint"0x10000"))
  }

  test("Literal syntax for invalid literals should not compile") {
    assert(
      clue(compileErrors("""delimiter"DERP"""")).contains(
        "A bootstring delimiter must be a single code point, the given value is invalid: DERP"
      )
    )
    assert(
      clue(compileErrors("""delimiter""""")).contains(
        "A bootstring delimiter must be a single code point, the given value is invalid:"
      )
    )
  }

  // Laws //

  checkAll("Order[Delimiter]", OrderTests[Delimiter].order)
  checkAll("Hash[Delimiter]", HashTests[Delimiter].hash)
}
