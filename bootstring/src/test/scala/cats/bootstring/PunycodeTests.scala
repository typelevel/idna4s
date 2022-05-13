package  cats.bootstring

import munit._

final class PunycodeTests extends ScalaCheckSuite {
  import PunycodeTests._

  test("The Punycode sample strings from RFC 3492 should encoded/decode correctly") {
    PunycodeTestStrings.foreach{(value: PunycodeTestString) =>
      val encoded: Either[String, String] = Bootstring.encodeRaw(BootstringParams.PunycodeParams)(value.raw)
      val decoded: Either[String, String] = Bootstring.decodeRaw(BootstringParams.PunycodeParams)(value.encoded)

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
