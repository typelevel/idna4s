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

import com.ibm.icu.text.IDNA
import java.lang.StringBuilder
import munit._
import org.scalacheck.Prop._
import org.scalacheck._
import org.typelevel.idna4s.core.uts46._
import org.typelevel.idna4s.scalacheck.all._

trait UTS46PlatformTests extends DisciplineSuite {

  private def configToIcu4jConfig(config: UTS46Config): Int = {
    val useStd3ASCIIRules: Int =
      if (config.useStd3ASCIIRules) IDNA.USE_STD3_RULES else IDNA.DEFAULT
    val checkBidi: Int = if (config.checkBidi) IDNA.CHECK_BIDI else IDNA.DEFAULT
    val checkJoiners: Int = if (config.checkJoiners) IDNA.CHECK_CONTEXTJ else IDNA.DEFAULT
    val transitionalProcessing: Int =
      if (config.transitionalProcessing) IDNA.DEFAULT
      else IDNA.NONTRANSITIONAL_TO_ASCII | IDNA.NONTRANSITIONAL_TO_UNICODE

    useStd3ASCIIRules | checkBidi | checkJoiners | transitionalProcessing
  }

  private def icu4jToASCII(config: UTS46Config, value: String): (IDNA.Info, String) = {
    val info: IDNA.Info = new IDNA.Info()

    (
      info,
      IDNA
        .getUTS46Instance(configToIcu4jConfig(config))
        .nameToASCII(value, new StringBuilder(value.size), info)
        .toString)
  }

  private val genIcu4jCompatibleConfig: Gen[UTS46Config] =
    Arbitrary
      .arbitrary[UTS46Config]
      .map(config => config.withCheckHyphens(true).withVerifyDNSLength(true))

  property(
    "idna4's uts46 implementation should agree with icu4j's uts46 implementation for arbitrary Strings") {
    forAll(genIcu4jCompatibleConfig, Arbitrary.arbitrary[String]) {
      (config: UTS46Config, name: String) =>
        val idna4stoASCIIResult: Either[UTS46.UTS46FailureException, String] =
          UTS46.toASCIIRaw(config)(name)
        val (icu4jInfo, icu4jToASCIIResult): (IDNA.Info, String) =
          icu4jToASCII(config, name)

        idna4stoASCIIResult match {
          case Left(errors) =>
            (icu4jInfo
              .hasErrors() ?= true) :| s"When idna4s UTS46 fails, so does icu4j: ${errors}."
          case Right(asciiName) =>
            ((icu4jInfo
              .hasErrors() ?= false) :| s"When idna4s UTS46 passes, so does icu4j: ${icu4jInfo.getErrors()}.") &&
            ((asciiName ?= icu4jToASCIIResult) :| "idna4s and icu4j produce the same result.")
        }
    }
  }
}
