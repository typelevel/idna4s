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
import org.typelevel.idna4s.core._

/**
 * A description of the status of the Unicode code point as determined by the IDNA mapping in
 * UTS-46.
 *
 * @see
 *   [[[[https://www.unicode.org/reports/tr46/#IDNA_Mapping_Table UTS-46 Section 5]]
 */
sealed abstract class CodePointStatus extends Serializable

object CodePointStatus {

  /**
   * Indicates the code point is Valid under UTS-46.
   *
   * Valid code points may be further separated as follows,
   *
   *   - Valid NV8, which are valid but excluded by IDNA 2008 for all domains for all versions
   *     of Unicode.
   *   - Valid XV8, which are valid but excluded by IDNA 2008 for the ''current'' version of
   *     Unicode.
   *   - Valid and not excluded by IDNA 2008 for any domain for any version of Unicode,
   *     including future ones.
   *
   * This distinction is not normative and does not affect the UTS-46 mapping.
   */
  sealed abstract class Valid extends CodePointStatus {
    def idna2008Status: Option[IDNA2008Status]

    final override def toString: String = s"Valid(idna2008Status = ${idna2008Status})"
  }

  object Valid {
    private[this] case object Always extends Valid {
      override val idna2008Status: Option[IDNA2008Status] = None
    }

    private[this] case object NV8 extends Valid {
      override val idna2008Status: Option[IDNA2008Status] = Some(IDNA2008Status.NV8)
    }

    private[this] case object XV8 extends Valid {
      override val idna2008Status: Option[IDNA2008Status] = Some(IDNA2008Status.XV8)
    }

    /** The always valid status. */
    def always: Valid = Always

    /** Valid, but excluded from IDNA 2008 for the current version of Unicode.
      */
    def nv8: Valid = NV8

    /** Valid, but excluded from IDNA 2008 for the all versions of Unicode.
      */
    def xv8: Valid = XV8

    /** Create a [[Valid]] instance, given some [[IDNA2008Status]] status.
      */
    def apply(idna2008Status: Option[IDNA2008Status]): Valid =
      idna2008Status.fold(
        always
      ) {
        case IDNA2008Status.NV8 => nv8
        case IDNA2008Status.XV8 => xv8
      }
  }

  /** Status indicating the code point is ignored in the UTS-46 mapping.
    */
  sealed abstract class Ignored extends CodePointStatus {
    final override def toString: String = "Ignored"
  }

  object Ignored {
    final private[this] case object IgnoredImpl extends Ignored

    val instance: Ignored = IgnoredImpl
  }

  /** Status indicating the code point is mapped to 1 or more code points in
    * UTS-46.
    */
  sealed abstract class Mapped extends CodePointStatus {

    /** The code points to which the input code point is mapped.
      */
    def mapping: NonEmptyList[CodePoint]

    final override def toString: String = s"Mapped(mapping = ${mapping})"
  }

  object Mapped {
    final private[this] case class MappedImpl(override val mapping: NonEmptyList[CodePoint])
        extends Mapped

    /** Create a mapping from a `NonEmptyList` of [[CodePoint]] values.
      */
    def of(mapping: NonEmptyList[CodePoint]): Mapped =
      MappedImpl(mapping)

    /** Create a mapping from a [[CodePoint]] values.
      */
    def one(mapping: CodePoint): Mapped =
      of(NonEmptyList.one(mapping))
  }

  sealed abstract class Deviation extends CodePointStatus {
    def mapping: List[CodePoint]

    final override def toString: String = s"Deviation(mapping = ${mapping})"
  }

  object Deviation {
    final private[this] case class DeviationImpl(override val mapping: List[CodePoint])
        extends Deviation

    val ignored: Deviation = DeviationImpl(Nil)

    def of(mapping: List[CodePoint]): Deviation =
      DeviationImpl(mapping)

    def one(mapping: CodePoint): Deviation =
      of(List(mapping))
  }

  sealed abstract class Disallowed extends CodePointStatus {
    final override def toString: String = "Disallowed"
  }

  object Disallowed {
    private[this] case object DisallowedImpl extends Disallowed

    val instance: Disallowed = DisallowedImpl
  }

  sealed abstract class Disallowed_STD3_Valid extends CodePointStatus {
    final override def toString: String = "Disallowed_STD3_Valid"
  }

  object Disallowed_STD3_Valid {
    private[this] case object Disallowed_STD3_ValidImpl extends Disallowed_STD3_Valid

    val instance: Disallowed_STD3_Valid = Disallowed_STD3_ValidImpl
  }

  sealed abstract class Disallowed_STD3_Mapped extends CodePointStatus {
    def mapping: NonEmptyList[CodePoint]

    final override def toString: String = s"Disallowed_STD3_Mapped(mapping = ${mapping})"
  }

  object Disallowed_STD3_Mapped {
    final private[this] case class Disallowed_STD3_MappedImpl(
        override val mapping: NonEmptyList[CodePoint])
        extends Disallowed_STD3_Mapped

    def of(mapping: NonEmptyList[CodePoint]): Disallowed_STD3_Mapped =
      Disallowed_STD3_MappedImpl(mapping)

    def one(mapping: CodePoint): Disallowed_STD3_Mapped =
      of(NonEmptyList.one(mapping))
  }

  sealed abstract class Unknown extends CodePointStatus

  object Unknown
}
