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
import com.ibm.icu.text.Normalizer2
import java.lang.StringBuilder
import java.text.Normalizer
import munit._
import org.scalacheck.Prop._
import org.scalacheck._
import org.typelevel.idna4s.core.uts46.CodePointMapper._
import org.typelevel.idna4s.core.uts46._

trait CodePointMapperPlatformTests extends DisciplineSuite {
  import CodePointMapperPlatformTests._

  // Note this test is flaky when idna4s and icu4j are targeting different
  // versions of Unicode.
  property("idna4s's uts-46 mapping step, should agree with icu4j's uts46-mapping step") {
    forAll { (s: String) =>
      // Note: We set useStd3ASCIIRules to false here, even though that is not
      // recommended as the standard behavior by UTS-46. The reason for this
      // is that icu4j's implementation handles useStd3ASCIIRules outside of
      // the Normalizer2 and we can't directly access that code.
      //
      // https://github.com/unicode-org/icu/blob/main/icu4j/release-72-rc/classes/core/src/com/ibm/icu/impl/UTS46.java#L366
      val idna4s: Either[MappingException, String] =
        mapCodePoints(s)
      val icu4j: String = icu4jUTS46Normalizer2.normalize(s, new StringBuilder(s.size)).toString

      idna4s.fold(_.partiallyMappedInput, identity) ?= icu4j
    }
  }

  property(
    "idna4s's uts-46 mapping step, should agree with icu4j's uts46-mapping step for ASCII strings") {
    // We have a special case fast path for ASCII code points. This tests that code path.
    forAll(Gen.asciiStr) { ascii =>
      val idna4s: Either[MappingException, String] =
        mapCodePoints(ascii)
      val icu4j: String =
        icu4jUTS46Normalizer2.normalize(ascii, new StringBuilder(ascii.size)).toString

      idna4s.fold(_.partiallyMappedInput, identity) ?= icu4j
    }
  }

  test("̸ࣶ should be consistent with icu4j") {
    val s: String = "̸ࣶ"
    val idna4s: Either[MappingException, String] =
      mapCodePoints(s)
    val icu4j: String =
      icu4jUTS46Normalizer2.normalize(s, new StringBuilder(s.size)).toString

    assertEquals(idna4s.fold(_.partiallyMappedInput, identity), icu4j)
  }

  test("a̸ࣶa should be consistent with icu4j") {
    val s: String = "a̸ࣶa"
    val idna4s: Either[MappingException, String] =
      mapCodePoints(s)
    val icu4j: String =
      icu4jUTS46Normalizer2.normalize(s, new StringBuilder(s.size)).toString

    assertEquals(idna4s.fold(_.partiallyMappedInput, identity), icu4j)
  }

  test("涇焑ꈛ਽৷降ٰࣶᕹ should be consistent with icu4j") {
    val s: String = "涇焑ꈛ਽৷降ٰࣶᕹ"
    val idna4s: Either[MappingException, String] =
      mapCodePoints(s)
    val icu4j: String =
      icu4jUTS46Normalizer2.normalize(s, new StringBuilder(s.size)).toString

    assertEquals(idna4s.fold(_.partiallyMappedInput, identity), icu4j)
  }
}

object CodePointMapperPlatformTests {
  val icu4jUTS46Normalizer2: Normalizer2 =
    Normalizer2.getInstance(null, "uts46", Normalizer2.Mode.COMPOSE)

  def nfc(value: String): String =
    Normalizer.normalize(value, Normalizer.Form.NFC)

  /**
   * Normalize the text with NFC before mapping the code points. This is ''not'' part of the
   * UTS-46 mapping step, but it is what the icu4j mapper does. icu4j is likely doing this
   * because this is required as part of UTS-46 as a whole, that is, their normalizer is doing
   * more than the UTS-46 mapping step.
   */
  def mapCodePoints(value: String): Either[MappingException, String] =
    CodePointMapper.mapCodePoints(false, false)(nfc(value))
}
