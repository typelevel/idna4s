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

package org.typelevel.idna4s.tests.uts46

import cats.syntax.all._
import munit._
import org.typelevel.idna4s.core._
import org.typelevel.idna4s.core.uts46._
import org.typelevel.idna4s.tests._

/**
 * Tests for the generated bidirectional class code.
 */
final class GeneratedBidirectionalClassTests extends DisciplineSuite {

  // Temporary
  //
  // We create an instance of the GeneratedBidirectionalClass so we can access
  // the generated method. In the near future, I want to expose a safer
  // interface for some of these generated mappings which will replace this.
  private[idna4s] object BidiTestClass extends GeneratedBidirectionalClass {
    def mapping(cp: Int): String =
      bidiTypeForCodePointInt(cp)
  }

  test("All code points should have a bidirectional mapping value") {
    reduceMapCodePoints { (cp: CodePoint) =>
      assert((BidiTestClass.mapping(clue(cp.value)) eq null) === false)
    }
  }

  test("@missing annotated code points are correctly overridden") {
    // If you are here this because this test started failing on a version of
    // Unicode > 15.0 please read this section. This test is very important,
    // but also may break between Unicode versions.
    //
    // We want to check that a code point which is only defined in
    // the @missing section has the correct value, but that another code point
    // which is defined in both the @missing and the normal section has the
    // normal value. This requires manually selecting a couple values to
    // check, in this case I've selected 07B0, 07B1, and 07B2.
    //
    // 07B0 should have the value of NSM, because it is overridden in the body
    // of DerivedBidiClass.txt
    //
    // 07B1 should have the value of AL, because it is overridden in the body
    // of DerivedBidiClass.txt. Note, in this case it has the same value as
    // the @missing default.
    //
    // 07B2 should have the value of AL, because it is only mentioned in
    // the @missing section
    //
    // Here are the relevant lines from DerivedBidiClass.txt for Unicode 15.0
    //
    // # @missing: 0600..07BF; Arabic_Letter
    // 07B1          ; AL # Lo       THAANA LETTER NAA
    // 07A6..07B0    ; NSM # Mn  [11] THAANA ABAFILI..THAANA SUKUN

    assertEquals(BidiTestClass.mapping(0x07b0), "NSM")
    assertEquals(BidiTestClass.mapping(0x07b1), "AL")
    assertEquals(BidiTestClass.mapping(0x07b2), "AL")
  }
}
