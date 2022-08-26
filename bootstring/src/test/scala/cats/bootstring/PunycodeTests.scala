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
