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
import munit._
import org.scalacheck.Prop._
import org.scalacheck._
import org.typelevel.idna4s.core.bootstring._
import org.typelevel.idna4s.core.syntax.all._
import org.typelevel.idna4s.scalacheck.all._

final class DampTests extends DisciplineSuite {

  property("Damp.fromInt/fromString should succeed for valid int32 value") {
    forAll((i: Int) =>
      if (i >= Damp.MinValue.value && i <= Damp.MaxValue.value) {
        Prop(Damp.fromInt(i).isRight)
        Prop(Damp.fromString(i.toString).isRight)
      } else {
        Prop(Damp.fromInt(i).isLeft)
        Prop(Damp.fromString(i.toString).isLeft)
      })
  }

  test("Damp.fromString fails for non integral strings") {
    assert(Damp.fromString("DERP").isLeft)
  }

  test("Literal syntax for valid damp values should compile") {
    assertEquals(damp"2", Damp.unsafeFromInt(2))
    assertEquals(damp"700", Damp.unsafeFromInt(700))
  }

  test("Literal syntax for invalid literals should not compile") {
    compileErrors("""damp"DERP"""").contains(
      "Given value is not a valid non-negative integral value")
    compileErrors("""damp"F"""").contains(
      "Given value is not a valid non-negative integral value")
  }

  // Laws //

  checkAll("Order[Damp]", OrderTests[Damp].order)
  checkAll("Hash[Damp]", HashTests[Damp].hash)
  checkAll("LowerBounded[Damp]", LowerBoundedTests[Damp].lowerBounded)
  checkAll("UpperBounded[Damp]", UpperBoundedTests[Damp].upperBounded)
}
