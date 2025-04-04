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

import cats._
import cats.collections.BitSet
import cats.data._
import cats.syntax.all._
import org.typelevel.idna4s.core._
import scala.annotation.tailrec
import scala.collection.immutable.IntMap
import scala.util.control.NoStackTrace
import java.nio.IntBuffer

object CodePointMapper extends GeneratedCodePointMapper {

  // Sentinel values for working with the asciiCodePointMap array
  final private val ASCII_DISALLOWED_STD3_VALID = -1
  final private val ASCII_VALID = -2

  /**
   * Array for the fast path of mapping ASCII code points (0x0, 0x7F) under UTS-46.
   *
   * Results are sentinel values or mapped code points.
   *
   * -1 = DISALLOWED_STD3_VALID
   * -2 = VALID
   *
   * Anything else is a mapped code point.
   */
  private val asciiCodePointMap: Array[Int] =
    (Range.inclusive(0x0, 0x2c).map(_ => ASCII_DISALLOWED_STD3_VALID) ++
      Range.inclusive(0x2d, 0x2e).map(_ => ASCII_VALID) ++
      Range.inclusive(0x2f, 0x2f).map(_ => ASCII_DISALLOWED_STD3_VALID) ++
      Range.inclusive(0x30, 0x39).map(_ => ASCII_VALID) ++
      Range.inclusive(0x3a, 0x40).map(_ => ASCII_DISALLOWED_STD3_VALID) ++
      Range
        .inclusive(0x41, 0x5a)
        .map(_.toChar.toLower.toInt) ++ /* Upper to lower case mapping */
      Range.inclusive(0x5b, 0x60).map(_ => ASCII_DISALLOWED_STD3_VALID) ++
      Range.inclusive(0x61, 0x7a).map(_ => ASCII_VALID) ++
      Range.inclusive(0x7b, 0x7f).map(_ => ASCII_DISALLOWED_STD3_VALID)).toArray

  /**
   * Map the code points in the given `String` according to the UTS-46 mapping tables as
   * described in section 5 of UTS-46. useStd3ASCIIRules is true and transitionalProcessing is
   * false.
   */
  def mapCodePoints(input: String): Either[MappingException, String] =
    mapCodePoints(true, false)(input)

  /**
   * As `mapCodePoints`, but throws on failure.
   */
  def unsafeMapCodePoints(input: String): String =
    mapCodePoints(input).fold(
      e => throw e,
      identity
    )

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

    @inline
    def put(acc: IntBuffer, value: Int): IntBuffer =
      maybeResize(acc, 1).put(value)

    @tailrec
    def loop(
        acc: IntBuffer,
        errors: Chain[CodePointMappingException],
        index: Int): Either[MappingException, String] =
      if (index >= len || index < 0 /* Overflow check */ ) {
        NonEmptyChain
          .fromChain(errors)
          .fold(
            Right(new String(acc.array, 0, acc.position)): Either[MappingException, String]
          )(errors => {
            Left(MappingException(errors, new String(acc.array(), 0, acc.position)))
          })
      } else {
        val value: Int = input.codePointAt(index)
        val nextIndex: Int = index + (if (value >= 0x10000) 2 else 1)
        val outIndex: Int = acc.position

        // ASCII fast path

        if (value <= 0x7f) {
          asciiCodePointMap(value) match {
            case ASCII_VALID =>
              loop(put(acc, value), errors, nextIndex)
            case ASCII_DISALLOWED_STD3_VALID =>
              // DISALLOWED_STD3_VALID
              if (useStd3ASCIIRules) {
                loop(
                  put(acc, value),
                  CodePointMappingException(
                    index,
                    outIndex,
                    "Disallowed code point in input.",
                    CodePoint.unsafeFromInt(value)) +: errors,
                  nextIndex
                )
              } else {
                loop(put(acc, value), errors, nextIndex)
              }
            case otherwise =>
              loop(put(acc, otherwise), errors, nextIndex)
          }
        } else {

          // We check valid then mapped first. This is for two reasons. First,
          // we want to prioritize fast execution on the successful code
          // path. Second, other than disallowed, valid/mapped make up the vast
          // majority of the domain.

          if (validAlways(value) || validNV8(value) || validXV8(value)) {
            // VALID
            loop(put(acc, value), errors, nextIndex)
          } else if (mapped.contains(value)) {
            // MAPPED
            loop(put(acc, mapped(value)), errors, nextIndex)
          } else if (mappedMultiCodePoints.contains(value)) {
            // MAPPED MULTI
            loop(
              mappedMultiCodePoints(value).foldLeft(acc) {
                case (acc, value) =>
                  put(acc, value)
              },
              errors,
              nextIndex)
          } else if (disallowed(value)) {
            // DISALLOWED
            loop(
              put(acc, value),
              CodePointMappingException(
                index,
                outIndex,
                "Disallowed code point in input.",
                CodePoint.unsafeFromInt(value)) +: errors,
              nextIndex
            )
          } else if (ignored(value)) {
            // IGNORED
            loop(acc, errors, nextIndex)
          } else if (deviationMapped.contains(value)) {
            // DEVIATION
            if (transitionalProcessing) {
              loop(put(acc, deviationMapped(value)), errors, nextIndex)
            } else {
              loop(put(acc, value), errors, nextIndex)
            }
          } else if (deviationMultiMapped.contains(value)) {
            // DEVIATION_MULTI
            if (transitionalProcessing) {
              loop(
                deviationMultiMapped(value).foldLeft(acc) {
                  case (acc, value) =>
                    put(acc, value)
                },
                errors,
                nextIndex)
            } else {
              loop(put(acc, value), errors, nextIndex)
            }
          } else if (disallowedSTD3Valid(value)) {
            // DISALLOWED_STD3_VALID
            if (useStd3ASCIIRules) {
              loop(
                put(acc, value),
                CodePointMappingException(
                  index,
                  outIndex,
                  "Disallowed code point in input.",
                  CodePoint.unsafeFromInt(value)) +: errors,
                nextIndex
              )
            } else {
              loop(put(acc, value), errors, nextIndex)
            }
          } else if (disallowedSTD3Mapped.contains(value)) {
            // DISALLOWED_STD3_MAPPED
            if (useStd3ASCIIRules) {
              loop(
                put(acc, value),
                CodePointMappingException(
                  index,
                  outIndex,
                  "Disallowed code point in input.",
                  CodePoint.unsafeFromInt(value)) +: errors,
                nextIndex
              )
            } else {
              loop(put(acc, disallowedSTD3Mapped(value)), errors, nextIndex)
            }
          } else if (disallowedSTD3MultiMapped.contains(value)) {
            // DISALLOWED_STD3_MAPPED_MULTI
            if (useStd3ASCIIRules) {
              loop(
                put(acc, value),
                CodePointMappingException(
                  index,
                  outIndex,
                  "Disallowed code point in input.",
                  CodePoint.unsafeFromInt(value)) +: errors,
                nextIndex
              )
            } else {
              loop(
                disallowedSTD3MultiMapped(value).foldLeft(acc) {
                  case (acc, value) =>
                    put(acc, value)
                },
                errors,
                nextIndex)
            }
          } else if (deviationIgnored(value)) {
            // DEVIATION_IGNORED
            if (transitionalProcessing) {
              loop(acc, errors, nextIndex)
            } else {
              loop(put(acc, value), errors, nextIndex)
            }
          } else {
            // Should be impossible
            throw new AssertionError(
              s"Code point does not map to any set (this is probably a bug in idna4s): $value"
            )
          }
        }
      }

    loop(IntBuffer.allocate(len + len / 2), Chain.empty, 0)
  }

  /**
   * As `mapCodePoints`, but throws on failure.
   */
  def unsafeMapCodePoints(useStd3ASCIIRules: Boolean, transitionalProcessing: Boolean)(
      input: String): String =
    mapCodePoints(useStd3ASCIIRules, transitionalProcessing)(input).fold(
      e => throw e,
      identity
    )

  /**
   * Determine the mapping of a code point according to the UTS-46 mapping tables as described
   * in section 5 of UTS-46.
   *
   * This is functionally the same as `mapCodePoints` and one could even use this to write
   * `mapCodePoints`, however `mapCodePoints` is optimized for bulk mapping a full `String` and
   * one should prefer that method for most use cases.
   *
   * @see
   *   [[https://www.unicode.org/reports/tr46/#IDNA_Mapping_Table UTS-46 Section 5]]
   */
  def mapCodePoint(codePoint: CodePoint): CodePointStatus = {
    import CodePointStatus._

    val value: Int = codePoint.value

    if (value <= 0x7f) {
      asciiCodePointMap(value) match {
        case ASCII_VALID =>
          Valid.always
        case ASCII_DISALLOWED_STD3_VALID =>
          Disallowed_STD3_Valid.instance
        case otherwise =>
          Mapped.one(CodePoint.unsafeFromInt(otherwise))
      }
    } else {

      if (validAlways(value)) {
        Valid.always
      } else if (validNV8(value)) {
        Valid.nv8
      } else if (validXV8(value)) {
        Valid.xv8
      } else if (mapped.contains(value)) {
        Mapped.one(CodePoint.unsafeFromInt(mapped(value)))
      } else if (mappedMultiCodePoints.contains(value)) {
        Mapped.of(mappedMultiCodePoints(value).map(CodePoint.unsafeFromInt))
      } else if (ignored(value)) {
        Ignored.instance
      } else if (deviationMapped.contains(value)) {
        Deviation.one(CodePoint.unsafeFromInt(deviationMapped(value)))
      } else if (deviationMultiMapped.contains(value)) {
        Deviation.of(deviationMultiMapped(codePoint.value).map(CodePoint.unsafeFromInt))
      } else if (disallowed(value)) {
        Disallowed.instance
      } else if (disallowedSTD3Valid(value)) {
        Disallowed_STD3_Valid.instance
      } else if (disallowedSTD3Mapped.contains(value)) {
        Disallowed_STD3_Mapped.one(CodePoint.unsafeFromInt(disallowedSTD3Mapped(value)))
      } else if (disallowedSTD3MultiMapped.contains(value)) {
        Disallowed_STD3_Mapped.of(disallowedSTD3MultiMapped(value).map(CodePoint.unsafeFromInt))
      } else if (deviationIgnored(value)) {
        Deviation.ignored
      } else {
        // Impossible

        throw new AssertionError(
          s"Code point does not map to any set (this is probably a bug in idna4s): $value"
        )
      }
    }
  }

  /**
   * As [[#mapIntCodePoint]], but throws an exception if the input is not a valid Unicode code
   * point.
   */
  def unsafeMapIntCodePoint(codePoint: Int): CodePointStatus =
    mapCodePoint(CodePoint.unsafeFromInt(codePoint))

  /**
   * As [[#mapCodePoint]], but takes an arbitrary `Int` value. This will fail the `Int` is not a
   * valid Unicode code point.
   */
  def mapIntCodePoint(codePoint: Int): Either[String, CodePointStatus] =
    Either.catchNonFatal(unsafeMapIntCodePoint(codePoint)).leftMap(_.getLocalizedMessage)

  /**
   * An error that is yielded when mapping an individual code point fails.
   */
  sealed abstract class CodePointMappingException extends IDNAException with NoStackTrace {

    /**
     * The index of the Unicode code point in the input where the failure occurred in the input
     * string.
     */
    def inputFailureIndex: Int

    /**
     * The index of Unicode code point in the partially mapped output string where the failure
     * occurred.
     *
     * This can deviate from the [[#inputFailureIndex]] because mapping of code points earlier
     * in the input might have resulted in what was 1 code point in the input becoming more than
     * 1 code point in the output.
     */
    def outputFailureIndex: Int

    /**
     * A description of why the failure occurred.
     */
    def message: String

    /**
     * The [[CodePoint]] which caused the failure.
     */
    def codePoint: CodePoint

    // final

    final override def getMessage: String =
      toString

    final override def toString: String =
      s"CodePointMappingException(message = $message, inputFailureIndex = $inputFailureIndex, outputFailureIndex = $outputFailureIndex, codePoint = $codePoint)"
  }

  object CodePointMappingException {
    final private[this] case class CodePointMappingExceptionImpl(
        override val inputFailureIndex: Int,
        override val outputFailureIndex: Int,
        override val message: String,
        override val codePoint: CodePoint)
        extends CodePointMappingException

    private[idna4s] def apply(
        inputFailureIndex: Int,
        outputFailureIndex: Int,
        message: String,
        codePoint: CodePoint): CodePointMappingException =
      CodePointMappingExceptionImpl(inputFailureIndex, outputFailureIndex, message, codePoint)

    implicit val hashAndOrderForCodePointMappingException
        : Hash[CodePointMappingException] with Order[CodePointMappingException] =
      new Hash[CodePointMappingException] with Order[CodePointMappingException] {
        override def hash(x: CodePointMappingException): Int = x.hashCode

        override def compare(x: CodePointMappingException, y: CodePointMappingException): Int =
          (x.inputFailureIndex, x.outputFailureIndex, x.message, x.codePoint)
            .compare((y.inputFailureIndex, y.outputFailureIndex, y.message, y.codePoint))
      }

    implicit val showForCodePointMappingException: Show[CodePointMappingException] =
      Show.fromToString
  }

  /**
   * An error which is yielded if attempting to map a sequence of Unicode code points with the
   * UTS-46 mapping algorithm fails.
   */
  sealed abstract class MappingException extends IDNAException with NoStackTrace {

    /**
     * One or more mapping [[CodePointMappingException]].
     *
     * Each [[CodePointMappingException]] describes the mapping failure of an independent code
     * point.
     */
    def errors: NonEmptyChain[CodePointMappingException]

    /**
     * The input string, mapped as much as was possible. Code points which were disallowed in
     * the input are left in place ''unchanged'', this makes this value unsafe to render in
     * error messages or back to the user. [[#renderablePartiallyMappedInput]] should be used to
     * render error messages to the user.
     *
     * This value is present because UTS-46 mandates that the algorithm continue to validity
     * checks, even in the event of failure, and the validity checks must operate on this
     * variant of the partially mapped input.
     */
    def unsafePartiallyMappedInput: String

    /**
     * The input string, mapped as much as was possible. Code points which failed replaced with
     * the Unicode replacement character � (0xFFFD). Returning this value on failure is mandated
     * by UTS-46.
     */
    def renderablePartiallyMappedInput: String

    final override def getMessage: String =
      toString

    final override def toString: String =
      s"MappingException(errors = ${errors}, renderablePartiallyMappedInput = ${renderablePartiallyMappedInput}, unsafePartiallyMappedInputHash = ${unsafePartiallyMappedInput.hash})"
  }

  object MappingException {
    final private[this] case class MappingExceptionImpl(
        override val errors: NonEmptyChain[CodePointMappingException],
        override val unsafePartiallyMappedInput: String
    ) extends MappingException {

      // We derive this lazily to avoid having to always have 2x memory
      // allocated. It's only needed when rendering errors.
      final override lazy val renderablePartiallyMappedInput: String = {
        val outBuffer: IntBuffer =
          codePointsAsBuffer(unsafePartiallyMappedInput)
        val len: Int = outBuffer.limit
        errors.iterator.foreach { error =>
          val _ = outBuffer.put(error.outputFailureIndex, ReplacementCharacter)
        }

        new String(outBuffer.array(), 0, len)
      }

      override def equals(that: Any): Boolean =
        that match {
          case that: MappingException =>
            (this: MappingException) === that
          case _ =>
            false
        }
    }

    private[idna4s] def apply(
        errors: NonEmptyChain[CodePointMappingException],
        unsafePartiallyMappedInput: String): MappingException =
      MappingExceptionImpl(errors, unsafePartiallyMappedInput)

    implicit val hashAndOrderForMappingException
        : Hash[MappingException] with Order[MappingException] =
      new Hash[MappingException] with Order[MappingException] {
        override def hash(x: MappingException): Int =
          x.hashCode

        override def compare(x: MappingException, y: MappingException): Int =
          x.errors.compare(y.errors) match {
            case 0 =>
              x.unsafePartiallyMappedInput.compare(y.unsafePartiallyMappedInput)
            case otherwise => otherwise
          }
      }
  }

  /**
   * A constant for the Unicode replacement character �.
   */
  final private[this] val ReplacementCharacter =
    0xfffd
}

/**
 * This is a base type which exists for the generated code.
 */
abstract private[uts46] class CodePointMapperBase {
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

  /**
   * The Unicode version the code point mapping functions target.
   *
   * @note
   *   UTS-46 is backwards compatible with future versions of Unicode. The only part of the
   *   mapping operation which can change between Unicode revisions is the disallowed character
   *   set. This includes the STD3 Valid and STD3 mapped variants. In other words, code points
   *   which were disallowed by some prior Unicode version may be made allowed in some future
   *   Unicode version or they may still be disallowed but have their mappings under STD3
   *   changed or they may be considered ''valid under STD3 only'', when they were formerly
   *   disallowed under STD3.
   */
  def unicodeVersion: String
}
