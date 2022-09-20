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

import scala.annotation.tailrec
import scala.annotation.switch
import cats.data._
import org.typelevel.idna4s.core._
import scala.util.control.NoStackTrace
import java.lang.StringBuilder
import scala.collection.immutable.IntMap
import scala.collection.immutable.BitSet

abstract private[uts46] class CodePointMapperBase {
  final protected val VALID = -1
  final protected val VALID_NV8 = -2
  final protected val VALID_XV8 = -3
  final protected val IGNORED = -4
  final protected val MULTI_MAPPED_CODE_POINT = -5
  final protected val DEVIATION = -6
  final protected val DEVIATION_MULTI = -7
  final protected val DISALLOWED = -8
  final protected val DISALLOWED_STD3_VALID = -9
  final protected val DISALLOWED_STD3_MAPPED = -10
  final protected val DISALLOWED_STD3_MULTI_MAPPED = -11
  final protected val DEVIATION_IGNORED = -12

  protected def validAlways: BitSet

  protected def validNV8: BitSet

  protected def validXV8: BitSet

  protected def ignored: BitSet

  protected def mappedMultiCodePoints: IntMap[NonEmptyList[Int]]

  protected def deviationMapped: IntMap[Int]

  protected def deviationMultiMapped: IntMap[NonEmptyList[Int]]

  protected def disallowed: BitSet

  protected def disallowedSTD3Valid: BitSet

  protected def disallowedSTD3Mapped: IntMap[Int]

  protected def disallowedSTD3MultiMapped: IntMap[NonEmptyList[Int]]

  protected def deviationIgnored: BitSet

  protected def mapped: IntMap[Int]
}

object CodePointMapper extends GeneratedCodePointMapper {

  /**
   * A representation of `mapped`, but it includes mapping for the sentinel values so that we
   * can quickly determine which secondary method to call in the even the code point is not in
   * the `mapped` set.
   */
  final private val mappedWithSentinels: IntMap[Int] =
    List[(Set[Int], Int)](
      validAlways -> VALID,
      validNV8 -> VALID_NV8,
      validXV8 -> VALID_XV8,
      ignored -> IGNORED,
      mappedMultiCodePoints.keySet -> MULTI_MAPPED_CODE_POINT,
      deviationMapped.keySet -> DEVIATION,
      deviationMultiMapped.keySet -> DEVIATION_MULTI,
      disallowed -> DISALLOWED,
      disallowedSTD3Valid -> DISALLOWED_STD3_VALID,
      disallowedSTD3Mapped.keySet -> DISALLOWED_STD3_MAPPED,
      disallowedSTD3MultiMapped.keySet -> DISALLOWED_STD3_MULTI_MAPPED,
      deviationIgnored -> DEVIATION_IGNORED
    ).foldLeft(mapped) {
      case (acc, (codePoints, sentinel)) =>
        codePoints.foldLeft(acc) {
          case (acc, value) =>
            acc.updated(value, sentinel)
        }
    }

  /**
   * Map the code points in the given `String` according to the UTS-46 mapping tables as
   * described in section 5 of UTS-46. useStd3ASCIIRules is true and transitionalProcessing is
   * false.
   */
  def mapCodePoints(input: String): Either[MappingException, String] =
    mapCodePoints(true, false)(input)

  /**
   * Map the code points in the given `String` according to the UTS-46 mapping tables as
   * described in section 5 of UTS-46. This is step 1 of processing an input `String` according
   * to UTS-46 for IDNA compatibility.
   *
   * @param useStd3ASCIIRules
   *   Whether or not to use STD3 ASCII rules. UTS-46 strongly recommends this be `true`.
   * @param transitionalProcessing
   *   Determines if the deviation characters should be mapped or considered valid. From UTS-46,
   *   "Transitional Processing should only be used immediately before a DNS lookup in the
   *   circumstances where the registry does not guarantee a strategy of bundling or blocking.
   *   Nontransitional Processing, which is fully compatible with IDNA2008, should be used in
   *   all other cases.
   *
   * @see
   *   [[https://www.unicode.org/reports/tr46/#IDNA_Mapping_Table UTS-46 Section 5]]
   */
  def mapCodePoints(useStd3ASCIIRules: Boolean, transitionalProcessing: Boolean)(
      input: String): Either[MappingException, String] = {
    val len: Int = input.length

    @tailrec
    def loop(
        acc: StringBuilder,
        errors: List[MappingError],
        index: Int): Either[MappingException, String] =
      if (index >= len) {
        NonEmptyList
          .fromList(errors)
          .fold(
            Right(acc.toString): Either[MappingException, String]
          )(errors => Left(MappingException(errors, acc.toString)))
      } else {
        val value: Int = input.codePointAt(index)
        val indexIncrement: Int = if (value >= 0x10000) 2 else 1

        // mappedWithSentinels should cover the entire Unicode codepoint
        // domain, so for any valid `String` this should always return a
        // value. Thus, we don't use .get to avoid the Option[Int] box.
        (mappedWithSentinels(value): @switch) match {
          case VALID | VALID_NV8 | VALID_XV8 =>
            loop(acc.appendCodePoint(value), errors, index + indexIncrement)
          case IGNORED =>
            loop(acc, errors, index + indexIncrement)
          case MULTI_MAPPED_CODE_POINT =>
            loop(
              mappedMultiCodePoints(value).foldLeft(acc) {
                case (acc, value) =>
                  acc.appendCodePoint(value)
              },
              errors,
              index + indexIncrement)
          case DEVIATION =>
            if (transitionalProcessing) {
              loop(acc.appendCodePoint(deviationMapped(value)), errors, index + indexIncrement)
            } else {
              loop(acc.appendCodePoint(value), errors, index + indexIncrement)
            }
          case DEVIATION_MULTI =>
            if (transitionalProcessing) {
              loop(
                deviationMultiMapped(value).foldLeft(acc) {
                  case (acc, value) =>
                    acc.appendCodePoint(value)
                },
                errors,
                index + indexIncrement)
            } else {
              loop(acc.appendCodePoint(value), errors, index + indexIncrement)
            }
          case DISALLOWED =>
            loop(
              acc.appendCodePoint(ReplacementCharacter),
              MappingError(
                index,
                "Disallowed code point in input.",
                CodePoint.unsafeFromInt(value)) :: errors,
              index + indexIncrement
            )
          case DISALLOWED_STD3_VALID =>
            if (useStd3ASCIIRules) {
              loop(
                acc.appendCodePoint(ReplacementCharacter),
                MappingError(
                  index,
                  "Disallowed code point in input.",
                  CodePoint.unsafeFromInt(value)) :: errors,
                index + indexIncrement
              )
            } else {
              loop(acc.appendCodePoint(value), errors, index + indexIncrement)
            }
          case DISALLOWED_STD3_MAPPED =>
            if (useStd3ASCIIRules) {
              loop(
                acc.appendCodePoint(ReplacementCharacter),
                MappingError(
                  index,
                  "Disallowed code point in input.",
                  CodePoint.unsafeFromInt(value)) :: errors,
                index + indexIncrement
              )
            } else {
              loop(
                acc.appendCodePoint(disallowedSTD3Mapped(value)),
                errors,
                index + indexIncrement)
            }
          case DISALLOWED_STD3_MULTI_MAPPED =>
            if (useStd3ASCIIRules) {
              loop(
                acc.appendCodePoint(ReplacementCharacter),
                MappingError(
                  index,
                  "Disallowed code point in input.",
                  CodePoint.unsafeFromInt(value)) :: errors,
                index + indexIncrement
              )
            } else {
              loop(
                disallowedSTD3MultiMapped(value).foldLeft(acc) {
                  case (acc, value) =>
                    acc.appendCodePoint(value)
                },
                errors,
                index + indexIncrement)
            }
          case DEVIATION_IGNORED =>
            loop(acc, errors, index + indexIncrement)
          case otherwise => // MAPPED or bug
            if (otherwise < 0) {
              throw new AssertionError(
                s"Unexpected mapping result (this is probably a bug in idna4s): $otherwise")
            } else {
              // This is a mapped code point
              loop(acc.appendCodePoint(otherwise), errors, index + indexIncrement)
            }
        }
      }

    loop(new StringBuilder(len), Nil, 0)
  }

  def mapCodePoint(codePoint: CodePoint): CodePointStatus = {
    import CodePointStatus._

    (mappedWithSentinels(codePoint.value): @switch) match {
      // Literals used rather than named constants because scalac won't
      // generate a switch with the named constants.
      case VALID => // VALID
        Valid.always
      case VALID_NV8 => // VALID_NV8
        Valid.nv8
      case VALID_XV8 => // VALID_XV8
        Valid.xv8
      case IGNORED => // IGNORED
        Ignored.instance
      case MULTI_MAPPED_CODE_POINT => // MULTI_MAPPED_CODE_POINT
        Mapped.of(mappedMultiCodePoints(codePoint.value).map(CodePoint.unsafeFromInt))
      case DEVIATION => // DEVIATION
        Deviation.one(CodePoint.unsafeFromInt(deviationMapped(codePoint.value)))
      case DEVIATION_MULTI => // DEVIATION_MULTI
        Deviation.of(deviationMultiMapped(codePoint.value).map(CodePoint.unsafeFromInt).toList)
      case DISALLOWED => // DISALLOWED
        Disallowed.instance
      case DISALLOWED_STD3_VALID => // DISALLOWED_STD3_VALID
        Disallowed_STD3_Valid.instance
      case DISALLOWED_STD3_MAPPED => // DISALLOWED_STD3_MAPPED
        Disallowed_STD3_Mapped.one(
          CodePoint.unsafeFromInt(disallowedSTD3Mapped(codePoint.value)))
      case DISALLOWED_STD3_MULTI_MAPPED => // DISALLOWED_STD3_MULTI_MAPPED
        Disallowed_STD3_Mapped.of(
          disallowedSTD3MultiMapped(codePoint.value).map(CodePoint.unsafeFromInt))
      case DEVIATION_IGNORED => // DEVIATION_IGNORED
        Deviation.ignored
      case otherwise =>
        // Mapped. There is no sentinel value for mapped, because in this case
        // a code point maps to a single new code point, we just return the
        // mapping directly.
        Mapped.one(CodePoint.unsafeFromInt(otherwise))
    }
  }

  def unsafeMapIntCodePoint(codePoint: Int): Either[String, CodePointStatus] =
    CodePoint.fromInt(codePoint).map(mapCodePoint)

  final private val ReplacementCharacter =
    0xfffd

  sealed abstract class MappingError extends Serializable {
    def failureIndex: Int

    def message: String

    def codePoint: CodePoint

    final override def toString: String =
      s"MappingError(message = $message, failureIndex = $failureIndex, codePoint = $codePoint)"
  }

  object MappingError {
    final private[this] case class MappingErrorImpl(
        override val failureIndex: Int,
        override val message: String,
        override val codePoint: CodePoint)
        extends MappingError

    def apply(failureIndex: Int, message: String, codePoint: CodePoint): MappingError =
      MappingErrorImpl(failureIndex, message, codePoint)
  }

  sealed abstract class MappingException extends RuntimeException with NoStackTrace {
    def errors: NonEmptyList[MappingError]

    def partiallyMappedInput: String

    final override def getMessage: String =
      toString

    final override def toString: String =
      s"MappingException(errors = ${errors}, partiallyMappedInput = ${partiallyMappedInput})"
  }

  object MappingException {
    final private[this] case class MappingExceptionImpl(
        override val errors: NonEmptyList[MappingError],
        override val partiallyMappedInput: String)
        extends MappingException

    def apply(
        errors: NonEmptyList[MappingError],
        partiallyMappedInput: String): MappingException =
      MappingExceptionImpl(errors, partiallyMappedInput)
  }
}
