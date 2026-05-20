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

package org.typelevel.idna4s.core.uts46

/**
 * Configuration object for UTS46 processing.
 *
 * See the member definitions for descriptions of how the affect UTS46.
 *
 * @see
 *   [[https://www.unicode.org/reports/tr46/#Processing]]
 * @see
 *   [[https://www.unicode.org/reports/tr46/#Validity_Criteria]]
 */
sealed abstract class UTS46Config extends Serializable {

  /**
   * From UTS46, section 4.1, validity criteria 2 and 3.
   *
   * {{{
   * If CheckHyphens, the label must not contain a U+002D HYPHEN-MINUS character in both the third and fourth positions.
   * If CheckHyphens, the label must neither begin nor end with a U+002D HYPHEN-MINUS character.
   * }}}
   *
   * For example,
   *
   * {{{
   * scala> val inputs: List[String] = List("-a", "a-", "ab--cd")
   * val inputs: List[String] = List(-a, a-, ab--cd)
   *
   * scala> inputs.map(UTS46.toASCIIRaw(config.withCheckHyphens(true))).foreach(println)
   * Left(UTS46FailureException(errors = Chain(LabelBeginsWithHyphenMinusException(getLocalizedMessage = Label begins with hyphen-minus (0x002d) and checkHyphens is on. UTS-46 forbids this.))))
   * Left(UTS46FailureException(errors = Chain(LabelEndsWithHyphenMinusException(getLocalizedMessage = Label ends with hyphen-minus (0x002d) and checkHyphens is on. UTS-46 forbids this.))))
   * Left(UTS46FailureException(errors = Chain(HyphenMinusInThirdAndFourthPositionException(getLocalizedMessage = Hyphen-minus (0x002d) code point found in positions 3 and 4 of label and checkHyphens is on. UTS-46 forbids this.))))
   *
   * scala> inputs.map(UTS46.toASCIIRaw(config.withCheckHyphens(false))).foreach(println)
   * Right(-a)
   * Right(a-)
   * Right(ab--cd)
   * }}}
   */
  def checkHyphens: Boolean
  def checkBidi: Boolean
  def checkJoiners: Boolean
  def useStd3ASCIIRules: Boolean
  def transitionalProcessing: Boolean
  def verifyDnsLength: Boolean

  def withCheckHyphens(value: Boolean): UTS46Config
  def withCheckBidi(value: Boolean): UTS46Config
  def withCheckJoiners(value: Boolean): UTS46Config
  def withUseStd3ASCIIRules(value: Boolean): UTS46Config
  def withTransitionalProcessing(value: Boolean): UTS46Config
  def withVerifyDNSLength(value: Boolean): UTS46Config

  final override def toString: String =
    s"UTS46Config(checkHyphens = ${checkHyphens}, checkBidi = ${checkBidi}, checkJoiners = ${checkJoiners}, useStd3ASCIIRules = ${useStd3ASCIIRules}, transitionalProcessing = ${transitionalProcessing}, verifyDnsLength = ${verifyDnsLength})"
}

object UTS46Config {

  val Strict: UTS46Config =
    UTS46Config(
      checkHyphens = true,
      checkBidi = true,
      checkJoiners = true,
      useStd3ASCIIRules = true,
      transitionalProcessing = false,
      verifyDnsLength = true
    )

  final private[this] case class UTS46ConfigImpl(
      override val checkHyphens: Boolean,
      override val checkBidi: Boolean,
      override val checkJoiners: Boolean,
      override val useStd3ASCIIRules: Boolean,
      override val transitionalProcessing: Boolean,
      override val verifyDnsLength: Boolean)
      extends UTS46Config {
    override def withCheckHyphens(value: Boolean): UTS46Config =
      copy(checkHyphens = value)
    override def withCheckBidi(value: Boolean): UTS46Config =
      copy(checkBidi = value)
    override def withCheckJoiners(value: Boolean): UTS46Config =
      copy(checkJoiners = value)
    override def withUseStd3ASCIIRules(value: Boolean): UTS46Config =
      copy(useStd3ASCIIRules = value)
    override def withTransitionalProcessing(value: Boolean): UTS46Config =
      copy(transitionalProcessing = value)
    override def withVerifyDNSLength(value: Boolean): UTS46Config =
      copy(verifyDnsLength = value)
  }

  def apply(
      checkHyphens: Boolean,
      checkBidi: Boolean,
      checkJoiners: Boolean,
      useStd3ASCIIRules: Boolean,
      transitionalProcessing: Boolean,
      verifyDnsLength: Boolean
  ): UTS46Config =
    UTS46ConfigImpl(
      checkHyphens,
      checkBidi,
      checkJoiners,
      useStd3ASCIIRules,
      transitionalProcessing,
      verifyDnsLength
    )
}
