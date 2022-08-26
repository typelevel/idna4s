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

final class PunycodeTests extends ScalaCheckSuite {
  import PunycodeTests._

  test("The Punycode sample strings from RFC 3492 should encoded/decode correctly") {
    PunycodeTestStrings.foreach { (value: PunycodeTestString) =>
      val encoded: Either[String, String] =
        Bootstring.encodeRaw(BootstringParams.PunycodeParams)(value.raw)
      val decoded: Either[String, String] =
        Bootstring.decodeRaw(BootstringParams.PunycodeParams)(value.encoded)

      assertEquals(
        encoded,
        Right(value.encoded)
      )

      assertEquals(
        decoded,
        Right(value.raw)
      )
    }
  }
}

object PunycodeTests {
  final case class PunycodeTestString(raw: String, encoded: String)

  val PunycodeTestStrings: List[PunycodeTestString] = List(
    PunycodeTestString(
      "\u0644\u064A\u0647\u0645\u0627\u0628\u062A\u0643\u0644\u0645\u0648\u0634\u0639\u0631\u0628\u064A\u061F",
      "egbpdaj6bu4bxfgehfvwxn"
    )
  )
}
