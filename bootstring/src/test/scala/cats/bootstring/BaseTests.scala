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
}
