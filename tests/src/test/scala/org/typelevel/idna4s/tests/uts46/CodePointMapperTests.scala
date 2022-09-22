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

import cats.data._
import munit._
import org.scalacheck.Prop._
import org.typelevel.idna4s.core._
import org.typelevel.idna4s.core.uts46._
import org.typelevel.idna4s.scalacheck.all._

final class CodePointMapperTests extends DisciplineSuite {

  property("Any valid unicode code point should have a code point status"){
    forAll((cp: CodePoint) =>
      assert(CodePointMapper.mapIntCodePoint(cp.value).isRight)
    )
  }

  test("Known valid input strings should map correctly"){
    assertEquals(CodePointMapper.mapCodePoints("valid"), Right("valid"))
  }

  test("Known invalid input strings should fail"){
    import CodePointMapper._
    val input: String = "$invalid"
    val unicodeReplacementCharacter: String = "\ufffd"
    assertEquals(CodePointMapper.mapCodePoints(input), Left(MappingException(NonEmptyList.of(CodePointMappingException(0, "Disallowed code point in input.", CodePoint.unsafeFromInt(input.codePointAt(0)))), s"${unicodeReplacementCharacter}invalid")))
  }
}
