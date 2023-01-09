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

import cats.data._
import cats.syntax.all._
import java.text.Normalizer
import org.typelevel.idna4s.core.CodePoint
import org.typelevel.idna4s.core.IDNAException
import org.typelevel.idna4s.core.bootstring._
import scala.annotation.tailrec
import scala.util.control.NoStackTrace

object UTS46 extends GeneratedUnicodeData with GeneratedJoiningType with GeneratedBidirectionalClass {

  def toASCIIRaw(config: UTS46Config)(value: String): Either[UTS46FailureException, String] =
    toASCIIRaw(checkHyphens = config.checkHyphens, checkBidi = config.checkBidi, checkJoiners = config.checkJoiners, useStd3ASCIIRules = config.useStd3ASCIIRules, transitionalProcessing = config.transitionalProcessing, verifyDnsLength = config.verifyDnsLength)(value)

  def toUnicodeRaw(config: UTS46Config)(value: String): Either[UTS46FailureException, String] =
    toUnicodeRaw(checkHyphens = config.checkHyphens, checkBidi = config.checkBidi, checkJoiners = config.checkJoiners, useStd3ASCIIRules = config.useStd3ASCIIRules, transitionalProcessing = config.transitionalProcessing)(value)

  def toASCIIRaw(checkHyphens: Boolean, checkBidi: Boolean, checkJoiners: Boolean, useStd3ASCIIRules: Boolean, transitionalProcessing: Boolean, verifyDnsLength: Boolean)(value: String): Either[UTS46FailureException, String] =
    process(checkHyphens = checkHyphens, checkBidi = checkBidi, checkJoiners = checkJoiners, useStd3ASCIIRules = useStd3ASCIIRules, transitionalProcessing = transitionalProcessing, value = value).flatMap(labels =>
      labels.nonEmptyTraverse(label =>
        encodeToPunycodeIfNeeded(label).fold(
          e => Ior.both(NonEmptyChain(e), label),
          label => Ior.right(label)
        )
      )
    ).flatMap(labels =>
      if (verifyDnsLength) {
        NonEmptyChain.fromChain(checkDnsLength(labels)).fold(
          Ior.right(labels): Ior[NonEmptyChain[IDNAException], NonEmptyChain[String]]
        )(errors =>
          Ior.both(errors, labels)
        )
      } else {
        Ior.right(labels)
      }
    ).fold(
      errors => Left(UTS46FailureException(errors, None)): Either[UTS46FailureException, String],
      labels => Right(labels.mkString_(FULL_STOP.toString)),
      {
        case (errors, labels) => Left(UTS46FailureException(errors, Some(labels.mkString_(FULL_STOP.toString))))
      }
    )

  def toUnicodeRaw(checkHyphens: Boolean, checkBidi: Boolean, checkJoiners: Boolean, useStd3ASCIIRules: Boolean, transitionalProcessing: Boolean)(value: String): Either[UTS46FailureException, String] =
    process(checkHyphens = checkHyphens, checkBidi = checkBidi, checkJoiners = checkJoiners, useStd3ASCIIRules = useStd3ASCIIRules, transitionalProcessing = transitionalProcessing, value = value).fold(
      errors => Left(UTS46FailureException(errors, None)): Either[UTS46FailureException, String],
      labels => Right(labels.mkString_(FULL_STOP.toString)),
      {
        case (errors, labels) => Left(UTS46FailureException(errors, Some(labels.mkString_(FULL_STOP.toString))))
      }
    )

  // A Bidi domain name is a domain name containing at least one character
  // with Bidi_Class R, AL, or AN. See [IDNA2008] RFC 5893, Section 1.4.

  private def isBidiDomainName(value: String): Boolean = {
    val len: Int = value.size
    @tailrec
    def loop(i: Int): Boolean =
      if (i >= len) {
        false
      } else {
        val cp: Int = value.codePointAt(i)
        val bidiCategory: String =
          Either.catchNonFatal(
            bidiTypeForCodePointInt(cp)
          ).fold(
            e => throw new RuntimeException(s"""Error getting ${String.format("%04X", cp)}""", e),
            identity
          )

        if (bidiCategory === "R" || bidiCategory === "AL" || bidiCategory === "AN") {
          true
        } else {
          val nextI: Int = if (cp >= 0x10000) i + 2 else i + 1
          loop(nextI)
        }
      }

    loop(0)
  }

  private def encodeToPunycodeIfNeeded(label: String): Either[Bootstring.BootstringException, String] = {
    val len: Int = label.size

    @tailrec
    def hasNonASCIIChar(charIndex: Int): Boolean =
      if (charIndex >= len) {
        false
      } else {
        val c: Char = label.charAt(charIndex)
        if (c.toInt > 127) {
          true
        } else {
          hasNonASCIIChar(charIndex + 1)
        }
      }

    if (hasNonASCIIChar(0)) {
      Bootstring.encodePunycodeRaw(label).map(label =>
        s"${PUNYCODE_PREFIX}${label}"
      )
    } else {
      Right(label)
    }
  }

  private def checkDnsLength(value: NonEmptyChain[String]): Chain[UTS46Exception] = {
    // TODO: Can be optimized to one traversal
    val emptyLabel: Boolean = value.last.isEmpty
    val dots: Long = value.length - 1L
    val totalSize: Long = value.reduceLeftTo(_.size){case (acc, value) => acc + value.size} + dots
    val totalSizeWithoutEmptyLabel: Long =
      if (emptyLabel && value.size > 1L) {
        // Subtract 1 for the empty label's dot.
        totalSize - 1L
      } else {
        totalSize
      }
    val withoutEmptyLabel: Chain[String] =
      if (emptyLabel) {
        value.init
      } else {
        value.toChain
      }

    val emptyLabelError: Chain[UTS46Exception] =
      if (emptyLabel) {
        Chain.one(UTS46Exception.EmptyRootLabelException)
      } else {
        Chain.empty
      }

    val domainLengthError: Chain[UTS46Exception] =
      if (totalSizeWithoutEmptyLabel > 253L) {
        Chain.one(UTS46Exception.DomainNameExceedsMaxLengthException(totalSizeWithoutEmptyLabel))
      } else {
        Chain.empty
      }

    withoutEmptyLabel.foldLeft(emptyLabelError ++ domainLengthError){
      case (errors, label) =>
        val len: Long = label.size.toLong
        if (len < 1L) {
          UTS46Exception.NonRootEmptyLabelException +: errors
        } else if (len > 63L) {
          UTS46Exception.LabelExceedsMaxLengthException(len) +: errors
        } else {
          errors
        }
    }
  }

  private def validInternal(checkHyphens: Boolean, checkBidi: Boolean, checkJoiners: Boolean, value: String): Chain[UTS46Exception] = {
    val len: Int = value.length()

    // We can skip the normalization check, this is always performed by either
    // toASCII or toUnicode. Thus we only need to do this for the public
    // validity check method. We also don't need to check for FULL_STOP code
    // points, as toASCII and toUnicode will split on them. We also do not
    // need to check step 6, that is taken care of by the mapping step in
    // toASCII and toUnicode.

    /* Check for validity criteria 2, HYPHEN_MINUS can not occur both positions 3
     * and 4. This should be called at most once when processing position 4 of
     * the input.
     */
    def checkHyphen34(errors: Chain[UTS46Exception], previousCodePoint: Option[Int], cp: Int): Chain[UTS46Exception] =
      if (cp === HYPHEN_MINUS_INT && previousCodePoint === Some(HYPHEN_MINUS_INT)) {
        errors :+ UTS46Exception.HyphenMinusInThirdAndFourthPositionException
      } else {
        errors
      }

    def checkNonJoinerPrevCodePoint(startCharIndex: Int): Boolean =
      if (startCharIndex <= 0) {
        false
      } else {
        val cp: Int = value.codePointBefore(startCharIndex)
        viramaCanonicalCombiningClassCodePoints(cp)
      }

    def checkNonJoinerBefore(startCharIndex: Int): Boolean = {

      @tailrec
      def loop(charIndex: Int): Boolean =
        if (charIndex <= 0) {
          false
        } else {
          val cp: Int = value.codePointBefore(charIndex)
          val nextIndex: Int = charIndex - (if (cp >= 0x10000) 2 else 1)

          if (isJoiningTypeL(cp) || isJoiningTypeD(cp)) {
            true
          } else {
            if (isJoiningTypeT(cp)) {
              loop(nextIndex)
            } else {
              false
            }
          }
        }

      loop(startCharIndex)
    }

    def checkNonJoinerAfter(startCharIndex: Int): Boolean = {

      @tailrec
      def loop(charIndex: Int): Boolean =
        if (charIndex >= len) {
          false
        } else {
          val cp: Int = value.codePointAt(charIndex)
          val nextIndex: Int = charIndex + (if (cp >= 0x10000) 2 else 1)

          if (charIndex === startCharIndex && cp === ZERO_WIDTH_NON_JOINER_INT) {
            // skip
            loop(nextIndex)
          } else {
            if (isJoiningTypeR(cp) || isJoiningTypeD(cp)) {
              true
            } else {
              if (isJoiningTypeT(cp)) {
                loop(nextIndex)
              } else {
                false
              }
            }
          }
        }

      loop(startCharIndex)
    }

    def checkFirstHyphen(errors: Chain[UTS46Exception], codePoint: Int): Chain[UTS46Exception] =
      if (checkHyphens && codePoint === HYPHEN_MINUS_INT) {
        errors :+ UTS46Exception.LabelBeginsWithHyphenMinusException
      } else {
        errors
      }

    def checkCombiningMark(errors: Chain[UTS46Exception], codePoint: Int): Chain[UTS46Exception] =
      if (combiningMarkCodePoints.apply(codePoint)) {
        errors :+ UTS46Exception.LabelStartsWithGeneralMarkException(codePoint)
      } else {
        errors
      }

    def checkFirstBidi(errors: Chain[UTS46Exception], codePoint: Int): (Chain[UTS46Exception], Option[BidiType]) =
      if (checkBidi) {
        bidiTypeForCodePointInt(codePoint) match {
          case "L" =>
            (errors, Some(BidiType.LTR))          case "R" | "AL" =>
            (errors, Some(BidiType.RTL(None)))
          case otherwise =>
            (errors :+ UTS46Exception.InvalidBidiTypeForFirstCodePointException(codePoint, otherwise), None)
        }
      } else {
        (errors, None)
      }

    def generalBidiCheck(errors: Chain[UTS46Exception], bidiType: BidiType, bidiNumberTypeError: BidiNumberTypeError, bidiEndLabelValid: BidiEndLabelValid, codePoint: Int): (BidiType, BidiNumberTypeError, BidiEndLabelValid, Chain[UTS46Exception]) =
      bidiType match {
        case BidiType.RTL(numberType) =>
          bidiTypeForCodePointInt(codePoint) match {
            case "AN" =>
              numberType match {
                case Some(BidiRTLNumberType.ArabicNumber) =>
                  (BidiType.RTL(numberType), bidiNumberTypeError, BidiEndLabelValid(true), errors)
                case Some(BidiRTLNumberType.EuropeanNumber) =>
                  // Set error
                  (BidiType.RTL(numberType), BidiNumberTypeError(true), BidiEndLabelValid(true), errors)
                case None =>
                  // Set number type
                  (BidiType.RTL(Some(BidiRTLNumberType.ArabicNumber)), bidiNumberTypeError, BidiEndLabelValid(true), errors)
              }
            case "EN" =>
              numberType match {
                case Some(BidiRTLNumberType.ArabicNumber) =>
                  // Set Error
                  (BidiType.RTL(numberType), BidiNumberTypeError(true), BidiEndLabelValid(true), errors)
                case Some(BidiRTLNumberType.EuropeanNumber) =>
                  (BidiType.RTL(numberType), bidiNumberTypeError, BidiEndLabelValid(true), errors)
                case None =>
                  // Set number Type
                  (BidiType.RTL(Some(BidiRTLNumberType.EuropeanNumber)), bidiNumberTypeError, BidiEndLabelValid(true), errors)
              }
            case "R" | "AL" =>
              (bidiType, bidiNumberTypeError, BidiEndLabelValid(true), errors)
            case "ES" | "CS" | "ET" | "ON" | "BN" =>
              (bidiType, bidiNumberTypeError, BidiEndLabelValid(false), errors)
            case "NSM" =>
              (bidiType, bidiNumberTypeError, bidiEndLabelValid, errors)
            case otherwise =>
              (bidiType, bidiNumberTypeError, BidiEndLabelValid(false), UTS46Exception.InvalidBidiTypeForRTLLabelException(codePoint, otherwise) +: errors)
          }
        case BidiType.LTR =>
          bidiTypeForCodePointInt(codePoint) match {
            case "L" | "EN" =>
              (bidiType, BidiNumberTypeError(false), BidiEndLabelValid(true), errors)
            case "ES" | "CS" | "ET" | "ON" | "BN" =>
              (bidiType, BidiNumberTypeError(false), BidiEndLabelValid(false), errors)
            case "NSM" =>
              (bidiType, BidiNumberTypeError(false), bidiEndLabelValid, errors)
            case otherwise =>
              (bidiType, BidiNumberTypeError(false), bidiEndLabelValid, UTS46Exception.InvalidBidiTypeForLTRLabelException(codePoint, otherwise) +: errors)
          }
      }

    def checkFirstCodePoint(errors: Chain[UTS46Exception], codePoint: Int): Chain[UTS46Exception] =
      checkCombiningMark(checkFirstHyphen(errors, codePoint), codePoint)

    def positionalChecks(errors: Chain[UTS46Exception], codePointIndex: Int, previousCodePoint: Option[Int], codePoint: Int): Chain[UTS46Exception] =
      codePointIndex match {
        case 0 =>
          checkFirstCodePoint(errors, codePoint)
        case 3 if checkHyphens =>
          checkHyphen34(errors, previousCodePoint, codePoint)
        case _ =>
          errors
      }

    def checkForJoiners(errors: Chain[UTS46Exception], previousCodePoint: Option[Int], charIndex: Int, codePoint: Int): Chain[UTS46Exception] =
      if (checkJoiners) {
        if (codePoint === ZERO_WIDTH_NON_JOINER_INT || codePoint === ZERO_WIDTH_JOINER_INT) {
          if (previousCodePoint.fold(false)(viramaCanonicalCombiningClassCodePoints.apply)) {
            errors
          } else if (codePoint === ZERO_WIDTH_NON_JOINER_INT) {
            if (checkNonJoinerPrevCodePoint(charIndex) || (checkNonJoinerBefore(charIndex) && checkNonJoinerAfter(charIndex))) {
              errors
            } else {
              errors :+ UTS46Exception.ContextJViolationForNonJoinerException
            }
          } else {
            errors :+ UTS46Exception.ContextJViolationForJoinerException
          }
        } else {
          errors
        }
      } else {
        errors
      }

    def checkFinalHyphen(errors: Chain[UTS46Exception], previousCodePoint: Option[Int]): Chain[UTS46Exception] =
      if (checkHyphens) {
        previousCodePoint.fold(
          errors
        ){
          case HYPHEN_MINUS_INT =>
            UTS46Exception.LabelEndsWithHyphenMinusException +: errors
          case _ =>
            errors
        }
      } else {
        errors
      }

    def checkFinalBidi(errors: Chain[UTS46Exception], bidiType: BidiType, bidiNumberTypeError: BidiNumberTypeError, bidiEndLabelValid: BidiEndLabelValid): Chain[UTS46Exception] =
      (if (bidiNumberTypeError.value) {
        UTS46Exception.MutuallyExclusiveBidiNumberTypesException +: errors
      } else {
        errors
      }) match {
        case errors =>
          if (bidiEndLabelValid.value) {
            errors
          } else {
            bidiType match {
              case _: BidiType.LTR.type =>
                UTS46Exception.LTRLabelDidNotEndWithCorrectBidiTypeException +: errors
              case _: BidiType.RTL =>
                UTS46Exception.RTLLabelDidNotEndWithCorrectBidiTypeException +: errors
            }
          }
      }

    // TODO: Optimization, consider making multiple variants of this loop for
    //       each permutation of condiditon.
    @tailrec
    def loop(
      errors: Chain[UTS46Exception],
      previousCodePoint: Option[Int],
      bidiType: Option[BidiType],
      bidiNumberTypeError: BidiNumberTypeError,
      bidiEndLabelValid: BidiEndLabelValid,
      codePointIndex: Int,
      charIndex: Int): Chain[UTS46Exception] =
      if (charIndex >= len) {
        // step 3 end check
        checkFinalHyphen(errors, previousCodePoint) match {
          case errors =>
            bidiType.fold(
              errors
            )(bidiType =>
              checkFinalBidi(errors, bidiType, bidiNumberTypeError, bidiEndLabelValid)
            )
        }
      } else {
        val cp: Int = value.codePointAt(codePointIndex)
        val nextCPIndex: Int = codePointIndex + 1
        val nextCharIndex: Int = charIndex + (if (cp >= 0x10000) 2 else 1)

        (if (codePointIndex === 0 && checkBidi) {
          checkFirstBidi(errors, cp)
        } else {
          (errors, bidiType)
        }) match {
          // Intentional shadow
          case (errors, bidiType) =>
            checkForJoiners(
              positionalChecks(errors, codePointIndex, previousCodePoint, cp),
              previousCodePoint,
              charIndex,
              cp
            ) match {
              case errors =>
                bidiType match {
                  case None =>
                    loop(errors, Some(cp), bidiType, bidiNumberTypeError, bidiEndLabelValid, nextCPIndex, nextCharIndex)
                  case Some(bidiType) =>
                    generalBidiCheck(errors, bidiType, bidiNumberTypeError, bidiEndLabelValid, cp) match {
                      case (bidiType, bidiNumberTypeError, bidiEndLabelValid, errors) =>
                        loop(errors, Some(cp), Some(bidiType), bidiNumberTypeError, bidiEndLabelValid, nextCPIndex, nextCharIndex)
                    }
                }
            }
        }
      }

    loop(Chain.empty, None, None, BidiNumberTypeError(false), BidiEndLabelValid(false), 0, 0)
  }

  private def process(checkHyphens: Boolean, checkBidi: Boolean, checkJoiners: Boolean, useStd3ASCIIRules: Boolean, transitionalProcessing: Boolean, value: String): Ior[NonEmptyChain[IDNAException], NonEmptyChain[String]] = {

    // The bidirectional rules apply if checkBidi is true _and_ the intput is
    // a bidi domain name.
    def shouldCheckBidi: Boolean =
      checkBidi && isBidiDomainName(value)

    def processLabel(label: String): Ior[NonEmptyChain[IDNAException], String] = {
      def validateLablel(label: String): Ior[NonEmptyChain[IDNAException], String] =
        NonEmptyChain.fromChain(validInternal(checkHyphens = checkHyphens, checkBidi = shouldCheckBidi, checkJoiners = checkJoiners, label)) match {
          case Some(nec) =>
            Ior.both(nec, label)
          case _ =>
            Ior.right(label)
        }

      if (label.startsWith(PUNYCODE_PREFIX)) {
        Bootstring.decodePunycodeRaw(label.drop(4)) match {
          case Left(e) =>
            Ior.both(NonEmptyChain.one(e), label)
          case Right(label) =>
            // When it is a Punycode label, we would always use
            // non-transitional processing for validation according to UTS-46,
            // however the validity check which requires non-transitional
            // processing (validity check 6) is not necessarily if the input
            // has already gone through the UTS-46 mapping step. It is only
            // applicable if we are applying the validity check to an
            // arbitrary string, which we never do.
            validateLablel(label)
        }
      } else {
        validateLablel(label)
      }
    }

    @tailrec
    def processLabels(labels: NonEmptyChain[String], acc: Ior[NonEmptyChain[IDNAException], NonEmptyChain[String]]): Ior[NonEmptyChain[IDNAException], NonEmptyChain[String]] =
      labels.uncons match {
        case (label, labels) =>
          acc.combine(processLabel(label).map(NonEmptyChain.one)) match {
            // Intentional Shadow
            case acc =>
              NonEmptyChain.fromChain(labels) match {
                case Some(labels) =>
                  processLabels(labels, acc)
                case _ =>
                  acc
              }
          }
      }

    @tailrec
    def toLabels(value: String, acc: Chain[String]): NonEmptyChain[String] =
      if (value.isEmpty) {
        // It is important that we don't ignore the empty label.
        NonEmptyChain.fromChainAppend(acc, value)
      } else {
        value.span(_ =!= FULL_STOP) match {
          case (label, rest) if rest.isEmpty =>
            NonEmptyChain.fromChainAppend(acc, label)
          case (label, rest) =>
            // First character in rest must be '.'
            toLabels(rest.tail, acc :+ label)
        }
      }

    Ior.fromEither(
      CodePointMapper.mapCodePoints(useStd3ASCIIRules, transitionalProcessing)(value).map(nfc)
    ).leftMap(NonEmptyChain.one[IDNAException]).flatMap(value =>
      (toLabels(value, Chain.empty).uncons match {
        case (label, labels) =>
          processLabel(label).map(NonEmptyChain.one) match {
            case acc =>
              NonEmptyChain.fromChain(labels) match {
                case Some(labels) =>
                  processLabels(labels, acc)
                case _ =>
                  acc
              }
          }
      })
    )
  }

  private def nfc(value: String): String =
    Normalizer.normalize(value, Normalizer.Form.NFC)

  private final val FULL_STOP = '\u002e'

  private final val PUNYCODE_PREFIX = "xn--"

  private final val HYPHEN_MINUS = '\u002d'

  private final val HYPHEN_MINUS_INT = HYPHEN_MINUS.toInt

  private final val ZERO_WIDTH_NON_JOINER = '\u200c'

  private final val ZERO_WIDTH_NON_JOINER_INT = ZERO_WIDTH_NON_JOINER.toInt

  private final val ZERO_WIDTH_JOINER = '\u200d'

  private final val ZERO_WIDTH_JOINER_INT = ZERO_WIDTH_NON_JOINER_INT.toInt

  private final case class BidiNumberTypeError(value: Boolean) extends AnyVal

  private final case class BidiEndLabelValid(value: Boolean) extends AnyVal

  private sealed abstract class BidiRTLNumberType extends Serializable

  private object BidiRTLNumberType {
    case object EuropeanNumber extends BidiRTLNumberType
    case object ArabicNumber extends BidiRTLNumberType
  }

  private sealed abstract class BidiType extends Serializable

  private object BidiType {
    case object LTR extends BidiType
    final case class RTL(numberType: Option[BidiRTLNumberType]) extends BidiType
  }

  sealed abstract class UTS46FailureException extends IDNAException with NoStackTrace {
    def errors: NonEmptyChain[IDNAException]

    def partiallyProcessedValue: Option[String]

    override final def getMessage: String =
      s"""Errors encountered during UTS-46 processing: ${errors.map(_.getLocalizedMessage).mkString_(", ")}"""

    override final def toString: String =
      s"UTS46FailureException(errors = ${errors})"
  }

  object UTS46FailureException {
    private[this] final case class UTS46FailureExceptionImpl(override val errors: NonEmptyChain[IDNAException], override val partiallyProcessedValue: Option[String]) extends UTS46FailureException

    private[UTS46] def apply(errors: NonEmptyChain[IDNAException], partiallyProcessedValue: Option[String]): UTS46FailureException =
      UTS46FailureExceptionImpl(errors, partiallyProcessedValue)
  }

  sealed abstract class UTS46Exception extends IDNAException with NoStackTrace

  object UTS46Exception {
    private[UTS46] case object HyphenMinusInThirdAndFourthPositionException extends UTS46Exception {
      override val getMessage: String =
        "Hyphen-minus (0x002d) code point found in positions 3 and 4 of label and checkHyphens is on. UTS-46 forbids this."

      override def toString: String =
        s"HyphenMinusInThirdAndFourthPositionException(getLocalizedMessage = ${getLocalizedMessage})"
    }

    private[UTS46] case object LabelBeginsWithHyphenMinusException extends UTS46Exception {
      override val getMessage: String =
        "Label begins with hyphen-minus (0x002d) and checkHyphens is on. UTS-46 forbids this."

      override def toString: String =
        s"LabelBeginsWithHyphenMinusException(getLocalizedMessage = ${getLocalizedMessage})"
    }

    private[UTS46] case object LabelEndsWithHyphenMinusException extends UTS46Exception {
      override val getMessage: String =
        "Label ends with hyphen-minus (0x002d) and checkHyphens is on. UTS-46 forbids this."

      override def toString: String =
        s"LabelEndsWithHyphenMinusException(getLocalizedMessage = ${getLocalizedMessage})"
    }

    private[UTS46] final case class LabelStartsWithGeneralMarkException(cp: Int) extends UTS46Exception {
      private def description: String =
        CodePoint.descriptionFromInt(cp)

      override def getMessage: String =
        s"The label starts with a code point which indicates a combining mark (General_Category=Mark in Unicode). This is forbidden by UTS-46: ${description}"

      override def toString: String =
        s"LabelStartsWithGeneralMarkException(cp = ${description}, getLocalizedMessage = ${getLocalizedMessage})"
    }

    private[UTS46] case object ContextJViolationForNonJoinerException extends UTS46Exception {
      override val getMessage: String =
        "ContextJ violation found for zero width non-joiner code point 0x200c. If present in a label, it must follow a code point which has a canonical combining class of Virama or it must follow a code point with a joining type of L (Left joining) or D (Dual joining) followed by zero or more code points with a joining type of T (transparent), then 0x200c, then be have zero or more code points after with a T joining type then a code point with a joining type of R (Right joining) or D."

      override def toString: String =
        s"ContextJViolationForNonJoinerException(getLocalizedMessage = ${getLocalizedMessage})"
    }

    private[UTS46] case object ContextJViolationForJoinerException extends UTS46Exception {
      override val getMessage: String =
        "ContextJ violation found for zero width joiner code point 0x200d. If present in a label, it must follow a code point which has a canonical combining class of Virama, but did not."

      override def toString: String =
        s"ContextJViolationForNonJoinerException(getLocalizedMessage = ${getLocalizedMessage})"
    }

    private[UTS46] final case class InvalidBidiTypeForFirstCodePointException(codePoint: Int, bidiType: String) extends UTS46Exception {
      override def getMessage: String =
        s"Invalid bidirectional type for first code point in label. Expected L, R, or AL, got ${bidiType}. Code point: ${CodePoint.descriptionFromInt(codePoint)}"

      override def toString: String =
        s"InvalidBidiTypeForFirstCodePointException(codePoint = ${codePoint}, bidiType = ${bidiType})"
    }

    private[UTS46] final case class InvalidBidiTypeForRTLLabelException(codePoint: Int, bidiType: String) extends UTS46Exception {
      override def getMessage: String =
        s"In an RTL label, only characters with the Bidi properties R, AL, AN, EN, ES, CS, ET, ON, BN, or NSM are allowed, but got ${bidiType} for code point: ${CodePoint.descriptionFromInt(codePoint)}"

      override def toString: String =
        s"InvalidBidiTypeForRTLLabelException(codePoint = ${CodePoint.descriptionFromInt(codePoint)}, bidiType = ${bidiType})"
    }

    private[UTS46] final case class InvalidBidiTypeForLTRLabelException(codePoint: Int, bidiType: String) extends UTS46Exception {
      override def getMessage: String =
        s"In an LTR label, only characters with the Bidi properties L, EN, ES, CS, ET, ON, BN, or NSM are allowed, but got ${bidiType} for code point: ${CodePoint.descriptionFromInt(codePoint)}"

      override def toString: String =
        s"InvalidBidiTypeForLTRLabelException(codePoint = ${CodePoint.descriptionFromInt(codePoint)}, bidiType = ${bidiType})"
    }

    private[UTS46] case object MutuallyExclusiveBidiNumberTypesException extends UTS46Exception {
      override val getMessage: String =
        "In an RTL label, if an EN is present, no AN may be present, and vice versa, however this label has both."

      override def toString: String =
        s"MutuallyExclusiveBidiNumberTypesException(getLocalizedMessage = ${getLocalizedMessage})"
    }

    private[UTS46] case object RTLLabelDidNotEndWithCorrectBidiTypeException extends UTS46Exception {
      override val getMessage: String =
        "In an RTL label, the end of the label must be a character with Bidi property R, AL, EN, or AN, followed by zero or more characters with Bidi property NSM, but this was not the case."

      override def toString: String =
        s"RTLLabelDidNotEndWithCorrectBidiTypeException(getLocalizedMessage = ${getLocalizedMessage})"
    }

    private[UTS46] case object LTRLabelDidNotEndWithCorrectBidiTypeException extends UTS46Exception {
      override val getMessage: String =
        "In an LTR label, the end of the label must be a character with Bidi property L or EN, followed by zero or more characters with Bidi property NSM, but this was not the case."

      override def toString: String =
        s"LTRLabelDidNotEndWithCorrectBidiTypeException(getLocalizedMessage = ${getLocalizedMessage})"
    }

    private[UTS46] case object NonRootEmptyLabelException extends UTS46Exception {
      override val getMessage: String = "An empty label was present but it was not the root label. This is forbidden."

      override def toString: String =
        s"NonRootEmptyLabelException(getLocalizedMessage = ${getLocalizedMessage})"
    }

    private[UTS46] case class LabelExceedsMaxLengthException(size: Long) extends UTS46Exception {
      // TODO: Include offending label? Need to check Unicode security recommendations.
      override def getMessage: String = s"A domain label is required to be between 1 and 63 characters when represented as ASCII, but got ${size}."

      override def toString: String =
        s"LabelExceedsMaxLengthException(size = ${size}, getLocalizedMessage = ${getLocalizedMessage})"
    }

    private[UTS46] case class DomainNameExceedsMaxLengthException(size: Long) extends UTS46Exception {
      // TODO: Include offending domain? Need to check Unicode security recommendations.
      override def getMessage: String = s"A domain name must be between 1 and 253 characters when represented as ASCII, but got ${size}."

      override def toString: String =
        s"DomainNameExceedsMaxLengthException(size = ${size}, getLocalizedMessage = ${getLocalizedMessage})"
    }

    private[UTS46] case object EmptyRootLabelException extends UTS46Exception {
      override val getMessage: String = "The domain ends with the empty root label. While this is a valid domain, UTS-46 forbids this notation."

      override def toString: String =
        s"EmptyRootLabelException(getLocalizedMessage = ${getLocalizedMessage})"
    }
  }
}
