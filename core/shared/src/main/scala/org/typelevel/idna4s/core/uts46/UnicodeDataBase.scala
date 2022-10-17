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

import scala.collection.immutable.IntMap
import cats.collections.BitSet

/**
 * Base class used with the code generation. The generated code which implements this trait
 * should be found in `UnicodeDataCodeGen.scala` in the `project` directory of the sbt build.
 * The generated implementation is expected to be named `GeneratedUnicodeData`.
 *
 * Values in this class come from the `UnicodeData.txt` file.
 *
 * @see
 *   [[https://www.unicode.org/Public/15.0.0/ucd/UnicodeData.txt]]
 */
private[uts46] trait UnicodeDataBase {

  /**
   * The set of Unicode code points which represent a combining mark, that is
   * General_Category=Mark.
   *
   * This is needed by the validation step of UTS-46. The General_Category=Mark category is a
   * meta-category made up of three sub categories, Spacing_Mark (Mc), Enclosing_Mark (Me), and
   * Nonspacing_Mark (Mn). General_Category=Mark mapping to these three sets is stable for
   * future revisions of Unicode.
   *
   * @see
   *   [[https://www.unicode.org/reports/tr46/#Validity_Criteria Validity_Criteria]]
   * @see
   *   [[https://www.unicode.org/policies/stability_policy.html]]
   */
  protected def combiningMarkCodePoints: BitSet

  /**
   * The set of Unicode code points which have a canonical combining class of Virama.
   *
   * These are used to check the ContextJ rules for the UTS-46 validity criteria.
   *
   * The ContextJ rules are part of IDNA 2008, which has an initial description, but it is
   * possible that the rules can change in the future. The current rules (still consistent with
   * IDNA 2008 at the time of writing) are defined by IANA.
   *
   * @see
   *   [[https://www.unicode.org/reports/tr46/#Validity_Criteria Validity_Criteria]]
   * @see
   *   [[https://www.iana.org/assignments/idna-tables-12.0.0/idna-tables-12.0.0.xhtml]]
   */
  protected def viramaCanonicalCombiningClassCodePoints: BitSet

  /**
   * The bidirectional category for all Unicode code points.
   *
   * These are used to check the bidi (bidirectional) rules for the UTS-46 validity criteria.
   * The actual rules are defined in RFC-5893 section 2.
   *
   * @see
   *   [[https://www.unicode.org/reports/tr46/#Validity_Criteria Validity_Criteria]]
   * @see
   *   [[https://www.rfc-editor.org/rfc/rfc5893.txt]]
   */
  protected def bidirectionalCategoryMap: IntMap[String]
}
