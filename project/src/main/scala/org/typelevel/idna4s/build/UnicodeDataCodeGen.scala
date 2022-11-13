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

package org.typelevel.idna4s.build

import cats._
import cats.data._
import cats.derived._
import cats.syntax.all._
import java.io.File
import java.net.URI
import java.net.URL
import sbt._
import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet
import scala.meta._
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.util.matching._

/**
 * Code generation utilities for code which is derived from `UnicodeData.txt`.
 *
 * It is intended that the `generate` method will be invoked from `CodeGen`. This is important
 * because we generate code from several different sources and we need to ensure that the
 * UnicodeVersion used is always the same.
 */
object UnicodeDataCodeGen {

  final private val BaseName = "UnicodeData"
  final private val BaseTypeName: String = s"${BaseName}Base"
  final private val GeneratedTypeName: String = s"Generated${BaseName}"

  /**
   * Generate the file for the functions, methods, and data derived from `UnicodeData.txt`.
   */
  private[build] def generate(
      dir: File,
      unicodeVersion: UnicodeVersion): Either[Throwable, File] = {
    val outputFile: File =
      dir / "org" / "typelevel" / "idna4s" / "core" / "uts46" / s"${GeneratedTypeName}.scala"

    rowsFromUnicodeVersion(unicodeVersion)
      .flatMap(unicodeData =>
        rowsToUnicodeData(unicodeData).leftMap(e => new RuntimeException(e)))
      .map { unicodeData => generatedSource(unicodeData) }
      .flatMap(tree => Either.catchNonFatal(IO.write(outputFile, tree.syntax)))
      .map(_ => outputFile)
  }

  /**
   * Generate the source code for the functions, methods, and data derived from
   * `UnicodeData.txt`.
   */
  private def generatedSource(unicodeData: UnicodeData[UnicodeCodePointInfomation]): Tree = {
    val combiningMarkCodePointsRHS: Term =
      combiningMarkExpression(unicodeData)

    source"""
package org.typelevel.idna4s.core.uts46

import cats.collections.BitSet
import scala.collection.immutable.IntMap

private[uts46] trait ${Type.Name(GeneratedTypeName)} extends ${Init(
        Type.Name(BaseTypeName),
        scala.meta.Name(""),
        Nil)} {
  override final protected lazy val combiningMarkCodePoints: BitSet = $combiningMarkCodePointsRHS

  ..${bidirectionalCategoryDefs(unicodeData)}
  ..${viramaCanonicalCombiningClassCodePointsDefs(unicodeData)}
}"""
  }

  /**
   * Parse the raw rows from a URL containing the `UnicodeData.txt` information.
   */
  private def rowsFromUrl(url: URL): Either[Throwable, Chain[UnicodeDataRow]] =
    Either.catchNonFatal(
      IO.readLinesURL(url).foldMap(value => Chain.one(UnicodeDataRow.unsafeFromLine(value)))
    )

  /**
   * Parse the raw rows for a given [[UnicodeVersion]].
   */
  private def rowsFromUnicodeVersion(
      unicodeVersion: UnicodeVersion): Either[Throwable, Chain[UnicodeDataRow]] =
    Either
      .catchNonFatal(
        URI
          .create(
            s"https://www.unicode.org/Public/${unicodeVersion.asString}/ucd/UnicodeData.txt")
          .toURL())
      .flatMap(
        rowsFromUrl
      )

  /**
   * ADT used to keep track of whether a line in `UnicodeData.txt` represents the start or end
   * of a code point range.
   */
  sealed abstract private[build] class RangeType extends Product with Serializable

  private[build] object RangeType {
    case object Start extends RangeType
    case object End extends RangeType

    implicit val hashAndOrderForRangeType: Hash[RangeType] with Order[RangeType] =
      new Hash[RangeType] with Order[RangeType] {
        override def hash(x: RangeType): Int =
          x.hashCode

        override def compare(x: RangeType, y: RangeType): Int =
          (x, y) match {
            case (Start, End) =>
              -1
            case (End, Start) =>
              1
            case (Start, Start) =>
              0
            case (End, End) =>
              0
          }
      }

    implicit def orderingInstance: Ordering[RangeType] =
      hashAndOrderForRangeType.toOrdering
  }

  /**
   * Newtype for the general category associated with a Unicode code point.
   */
  final private[build] case class GeneralCategory(value: String) extends AnyVal {
    def isCombiningMark: Boolean =
      GeneralCategory.combiningMarkCategories.contains(value.toLowerCase)
  }

  private[build] object GeneralCategory {
    val combiningMarkCategories: SortedSet[String] =
      SortedSet("Mc", "Me", "Mn").map(_.toLowerCase)

    implicit val hashAndOrderForGeneralCategory
        : Hash[GeneralCategory] with Order[GeneralCategory] =
      new Hash[GeneralCategory] with Order[GeneralCategory] {
        override def hash(x: GeneralCategory): Int =
          x.hashCode

        override def compare(x: GeneralCategory, y: GeneralCategory): Int =
          x.value.compare(y.value)
      }

    implicit def orderingInstance: Ordering[GeneralCategory] =
      hashAndOrderForGeneralCategory.toOrdering
  }

  /**
   * Newtype for the name associated with a Unicode code point or range of code points.
   */
  final private[build] case class Name(value: String) extends AnyVal

  private[build] object Name {
    implicit val hashAndOrderForName: Hash[Name] with Order[Name] =
      new Hash[Name] with Order[Name] {
        override def hash(x: Name): Int =
          x.hashCode

        override def compare(x: Name, y: Name): Int =
          x.value.compare(y.value)
      }

    implicit def orderingInstance: Ordering[Name] =
      hashAndOrderForName.toOrdering
  }

  /**
   * Newtype for the canonical combining class associated with a Unicode code point or range of
   * code points.
   */
  final private[build] case class CanonicalCombiningClass(value: Int) extends AnyVal {
    def isVirama: Boolean = value === 9
  }

  private[build] object CanonicalCombiningClass {

    def fromString(value: String): Either[String, CanonicalCombiningClass] =
      Either
        .catchNonFatal(value.toInt)
        .bimap(
          _.getLocalizedMessage,
          CanonicalCombiningClass.apply
        )

    def unsafeFromString(value: String): CanonicalCombiningClass =
      fromString(value).fold(
        e => throw new IllegalArgumentException(e),
        identity
      )

    implicit val hashAndOrderForCanonicalCombiningClass
        : Hash[CanonicalCombiningClass] with Order[CanonicalCombiningClass] =
      new Hash[CanonicalCombiningClass] with Order[CanonicalCombiningClass] {
        override def hash(x: CanonicalCombiningClass): Int =
          x.hashCode

        override def compare(x: CanonicalCombiningClass, y: CanonicalCombiningClass): Int =
          x.value.compare(y.value)
      }

    implicit def orderingInstance: Ordering[CanonicalCombiningClass] =
      hashAndOrderForCanonicalCombiningClass.toOrdering
  }

  /**
   * Newtype for the bidirectional (bidi) category associated with a Unicode code point or range
   * of code points.
   */
  final private[build] case class BidirectionalCategory(value: String) extends AnyVal

  private[build] object BidirectionalCategory {
    implicit val hashAndOrderForBidirectionalCategory
        : Hash[BidirectionalCategory] with Order[BidirectionalCategory] =
      new Hash[BidirectionalCategory] with Order[BidirectionalCategory] {
        override def hash(x: BidirectionalCategory): Int =
          x.hashCode

        override def compare(x: BidirectionalCategory, y: BidirectionalCategory): Int =
          x.value.compare(y.value)
      }

    implicit def orderingInstance: Ordering[BidirectionalCategory] =
      hashAndOrderForBidirectionalCategory.toOrdering
  }

  /**
   * Newtype for the character decomposition mapping for a Unicode code point or range of code
   * points.
   */
  final private[build] class CharacterDecompositionMapping private (val value: String)
      extends AnyVal

  private[build] object CharacterDecompositionMapping {
    private def apply(value: String): CharacterDecompositionMapping =
      new CharacterDecompositionMapping(value)

    def fromString(value: String): Either[String, CharacterDecompositionMapping] =
      if (value.nonEmpty) {
        Right(CharacterDecompositionMapping(value))
      } else {
        Left("CharacterDecompositionMapping values must be non-empty Strings.")
      }

    def unsafeFromString(value: String): CharacterDecompositionMapping =
      fromString(value).fold(
        e => throw new IllegalArgumentException(e),
        identity
      )

    implicit val hashAndOrderForCharacterDecompositionMapping
        : Hash[CharacterDecompositionMapping] with Order[CharacterDecompositionMapping] =
      new Hash[CharacterDecompositionMapping] with Order[CharacterDecompositionMapping] {
        override def hash(x: CharacterDecompositionMapping): Int =
          x.hashCode

        override def compare(
            x: CharacterDecompositionMapping,
            y: CharacterDecompositionMapping): Int =
          x.value.compare(y.value)
      }

    implicit def orderingInstance: Ordering[CharacterDecompositionMapping] =
      hashAndOrderForCharacterDecompositionMapping.toOrdering
  }

  /**
   * Newtype for the decimal digit value associated with a Unicode code point.
   */
  final private[build] class DecimalDigitValue private (val value: Int) extends AnyVal

  private[build] object DecimalDigitValue {
    private def apply(value: Int): DecimalDigitValue =
      new DecimalDigitValue(value)

    def unsafeFromInt(value: Int): DecimalDigitValue =
      if (value >= 0 && value <= 9) {
        DecimalDigitValue(value)
      } else {
        throw new IllegalArgumentException(
          s"Expected decimal digit value to be [0,9], but was: ${value}")
      }

    def unsafeFromString(value: String): Option[DecimalDigitValue] =
      if (value.nonEmpty) {
        Some(unsafeFromInt(value.toInt))
      } else {
        None
      }

    implicit val hashAndOrderForDecimalDigitValue
        : Hash[DecimalDigitValue] with Order[DecimalDigitValue] =
      new Hash[DecimalDigitValue] with Order[DecimalDigitValue] {
        override def hash(x: DecimalDigitValue): Int =
          x.hashCode

        override def compare(x: DecimalDigitValue, y: DecimalDigitValue): Int =
          x.value.compare(y.value)
      }

    implicit def orderingInstance: Ordering[DecimalDigitValue] =
      hashAndOrderForDecimalDigitValue.toOrdering
  }

  /**
   * Newtype for the digit value associated with a Unicode code point.
   */
  final private[build] class DigitValue private (val value: Int) extends AnyVal

  private[build] object DigitValue {
    private def apply(value: Int): DigitValue =
      new DigitValue(value)

    def unsafeFromInt(value: Int): DigitValue =
      if (value >= 0) {
        DigitValue(value)
      } else {
        throw new IllegalArgumentException(
          s"Expected digit value to be >= 0, but was: ${value}")
      }

    def unsafeFromString(value: String): Option[DigitValue] =
      if (value.nonEmpty) {
        Some(unsafeFromInt(value.toInt))
      } else {
        None
      }

    implicit val hashAndOrderForDigitValue: Hash[DigitValue] with Order[DigitValue] =
      new Hash[DigitValue] with Order[DigitValue] {
        override def hash(x: DigitValue): Int =
          x.hashCode

        override def compare(x: DigitValue, y: DigitValue): Int =
          x.value.compare(y.value)
      }

    implicit def orderingInstance: Ordering[DigitValue] =
      hashAndOrderForDigitValue.toOrdering
  }

  /**
   * Newtype for the numeric value for a Unicode code point.
   */
  final private[build] class NumericValue private (val value: String) extends AnyVal

  private[build] object NumericValue {
    private def apply(value: String): NumericValue =
      new NumericValue(value)

    def fromString(value: String): Either[String, NumericValue] =
      if (value.nonEmpty) {
        Right(NumericValue(value))
      } else {
        Left("Numeric values may not be empty.")
      }

    def unsafeFromString(value: String): NumericValue =
      fromString(value).fold(
        e => throw new IllegalArgumentException(e),
        identity
      )

    implicit val hashAndOrderForNumericValue: Hash[NumericValue] with Order[NumericValue] =
      new Hash[NumericValue] with Order[NumericValue] {
        override def hash(x: NumericValue): Int =
          x.hashCode

        override def compare(x: NumericValue, y: NumericValue): Int =
          x.value.compare(y.value)
      }

    implicit def orderingInstance: Ordering[NumericValue] =
      hashAndOrderForNumericValue.toOrdering
  }

  /**
   * Newtype for whether or not the Unicode code point represents a character which is a
   * "mirrored" character in bidirectional text.
   */
  final private[build] case class Mirrored(val value: Boolean) extends AnyVal

  private[build] object Mirrored {
    def unsafeFromString(value: String): Mirrored =
      if (value === "Y") {
        Mirrored(true)
      } else if (value === "N") {
        Mirrored(false)
      } else {
        throw new IllegalArgumentException(s"Invalid value for Mirrored: ${value}")
      }

    implicit val hashAndOrderForMirrored: Hash[Mirrored] with Order[Mirrored] =
      new Hash[Mirrored] with Order[Mirrored] {
        override def hash(x: Mirrored): Int =
          x.hashCode

        override def compare(x: Mirrored, y: Mirrored): Int =
          x.value.compare(y.value)
      }

    implicit def orderingInstance: Ordering[Mirrored] =
      hashAndOrderForMirrored.toOrdering
  }

  /**
   * Newtype for the old name as published in Unicode 1.0 or ISO 6429 names for control
   * functions. This field is empty unless it is significantly different from the current name
   * for the character. No longer used in code chart production.
   */
  final private[build] class Unicode1Name private (val value: String) extends AnyVal

  private[build] object Unicode1Name {
    private def apply(value: String): Unicode1Name =
      new Unicode1Name(value)

    def fromString(value: String): Either[String, Unicode1Name] =
      if (value.nonEmpty) {
        Right(Unicode1Name(value))
      } else {
        Left("Unicode1Name values may not be empty.")
      }

    def unsafeFromString(value: String): Unicode1Name =
      fromString(value).fold(
        e => throw new IllegalArgumentException(e),
        identity
      )

    implicit val hashAndOrderForUnicode1Name: Hash[Unicode1Name] with Order[Unicode1Name] =
      new Hash[Unicode1Name] with Order[Unicode1Name] {
        override def hash(x: Unicode1Name): Int =
          x.hashCode

        override def compare(x: Unicode1Name, y: Unicode1Name): Int =
          x.value.compare(y.value)
      }

    implicit def orderingInstance: Ordering[Unicode1Name] =
      hashAndOrderForUnicode1Name.toOrdering
  }

  /**
   * Newtype for the ISO 10646 comment field. It was used for notes that appeared in parentheses
   * in the 10646 names list, or contained an asterisk to mark an Annex P note. As of Unicode
   * 5.2.0, this field no longer contains any non-null values.
   */
  final private[build] class ISO10646Comment private (val value: String) extends AnyVal

  private[build] object ISO10646Comment {
    private def apply(value: String): ISO10646Comment =
      new ISO10646Comment(value)

    def fromString(value: String): Either[String, ISO10646Comment] =
      if (value.nonEmpty) {
        Right(ISO10646Comment(value))
      } else {
        Left("ISO10646Comment values may not be empty.")
      }

    def unsafeFromString(value: String): ISO10646Comment =
      fromString(value).fold(
        e => throw new IllegalArgumentException(e),
        identity
      )

    implicit val hashAndOrderForISO10646Comment
        : Hash[ISO10646Comment] with Order[ISO10646Comment] =
      new Hash[ISO10646Comment] with Order[ISO10646Comment] {
        override def hash(x: ISO10646Comment): Int =
          x.hashCode

        override def compare(x: ISO10646Comment, y: ISO10646Comment): Int =
          x.value.compare(y.value)
      }

    implicit def orderingInstance: Ordering[ISO10646Comment] =
      hashAndOrderForISO10646Comment.toOrdering
  }

  /**
   * Newtype for the simple uppercase mapping (single character result). If a character is part
   * of an alphabet with case distinctions, and has a simple uppercase equivalent, then the
   * uppercase equivalent is in this field. The simple mappings have a single character result,
   * where the full mappings may have multi-character results. For more information, see Case
   * and Case Mapping.
   */
  final private[build] class UppercaseMapping private (val value: String) extends AnyVal

  private[build] object UppercaseMapping {
    private def apply(value: String): UppercaseMapping =
      new UppercaseMapping(value)

    def fromString(value: String): Either[String, UppercaseMapping] =
      if (value.nonEmpty) {
        Right(UppercaseMapping(value))
      } else {
        Left("UppercaseMapping values may not be empty.")
      }

    def unsafeFromString(value: String): UppercaseMapping =
      fromString(value).fold(
        e => throw new IllegalArgumentException(e),
        identity
      )

    implicit val hashAndOrderForUppercaseMapping
        : Hash[UppercaseMapping] with Order[UppercaseMapping] =
      new Hash[UppercaseMapping] with Order[UppercaseMapping] {
        override def hash(x: UppercaseMapping): Int =
          x.hashCode

        override def compare(x: UppercaseMapping, y: UppercaseMapping): Int =
          x.value.compare(y.value)
      }

    implicit def orderingInstance: Ordering[UppercaseMapping] =
      hashAndOrderForUppercaseMapping.toOrdering
  }

  /**
   * Newtype for Simple lowercase mapping.
   */
  final private[build] class LowercaseMapping private (val value: String) extends AnyVal

  private[build] object LowercaseMapping {
    private def apply(value: String): LowercaseMapping =
      new LowercaseMapping(value)

    def fromString(value: String): Either[String, LowercaseMapping] =
      if (value.nonEmpty) {
        Right(LowercaseMapping(value))
      } else {
        Left("LowercaseMapping values may not be empty.")
      }

    def unsafeFromString(value: String): LowercaseMapping =
      fromString(value).fold(
        e => throw new IllegalArgumentException(e),
        identity
      )

    implicit val hashAndOrderForLowercaseMapping
        : Hash[LowercaseMapping] with Order[LowercaseMapping] =
      new Hash[LowercaseMapping] with Order[LowercaseMapping] {
        override def hash(x: LowercaseMapping): Int =
          x.hashCode

        override def compare(x: LowercaseMapping, y: LowercaseMapping): Int =
          x.value.compare(y.value)
      }

    implicit def orderingInstance: Ordering[LowercaseMapping] =
      hashAndOrderForLowercaseMapping.toOrdering
  }

  /**
   * Newtype for Simple titlecase mapping (single character result). Note: If this field is
   * null, then the Simple_Titlecase_Mapping is the same as the Simple_Uppercase_Mapping for
   * this character.
   */
  final private[build] class TitlecaseMapping private (val value: String) extends AnyVal

  private[build] object TitlecaseMapping {
    private def apply(value: String): TitlecaseMapping =
      new TitlecaseMapping(value)

    def fromString(value: String): Either[String, TitlecaseMapping] =
      if (value.nonEmpty) {
        Right(TitlecaseMapping(value))
      } else {
        Left("TitlecaseMapping values may not be empty.")
      }

    def unsafeFromString(value: String): TitlecaseMapping =
      fromString(value).fold(
        e => throw new IllegalArgumentException(e),
        identity
      )

    implicit val hashAndOrderForTitlecaseMapping
        : Hash[TitlecaseMapping] with Order[TitlecaseMapping] =
      new Hash[TitlecaseMapping] with Order[TitlecaseMapping] {
        override def hash(x: TitlecaseMapping): Int =
          x.hashCode

        override def compare(x: TitlecaseMapping, y: TitlecaseMapping): Int =
          x.value.compare(y.value)
      }

    implicit def orderingInstance: Ordering[TitlecaseMapping] =
      hashAndOrderForTitlecaseMapping.toOrdering
  }

  /**
   * The information about a code point or range of code points as described in
   * `UnicodeData.txt`
   */
  final private[build] case class UnicodeCodePointInfomation(
      name: Name,
      generalCategory: GeneralCategory,
      canonicalCombiningClass: CanonicalCombiningClass,
      bidirectionalCategory: BidirectionalCategory,
      characterDecompositionMapping: Option[CharacterDecompositionMapping],
      decimalDigitValue: Option[DecimalDigitValue],
      digitValue: Option[DigitValue],
      numericValue: Option[NumericValue],
      mirrored: Mirrored,
      unicode1Name: Option[Unicode1Name],
      iso10646Comment: Option[ISO10646Comment],
      uppercaseMapping: Option[UppercaseMapping],
      lowercaseMapping: Option[LowercaseMapping],
      titlecaseMapping: Option[TitlecaseMapping]
  )

  private[build] object UnicodeCodePointInfomation {
    implicit val hashAndOrderForUnicodeCodePointInfomation
        : Hash[UnicodeCodePointInfomation] with Order[UnicodeCodePointInfomation] = {
      val order: Order[UnicodeCodePointInfomation] = semiauto.order

      new Hash[UnicodeCodePointInfomation] with Order[UnicodeCodePointInfomation] {
        override def hash(x: UnicodeCodePointInfomation): Int = x.hashCode

        override def compare(
            x: UnicodeCodePointInfomation,
            y: UnicodeCodePointInfomation): Int =
          order.compare(x, y)
      }
    }

    implicit def orderingInstance: Ordering[UnicodeCodePointInfomation] =
      hashAndOrderForUnicodeCodePointInfomation.toOrdering
  }

  /**
   * A representation of a single row from `UnicodeData.txt`.
   */
  final private[build] case class UnicodeDataRow(
      codeValue: CodePoint,
      rangeType: Option[RangeType],
      unicodeCodePointInformation: UnicodeCodePointInfomation
  )

  private[build] object UnicodeDataRow {

    /**
     * Regex for parsing a row from `UnicodeData.txt`.
     */
    private val rowRegex: Regex =
      """([^;]*);([^;]*);([^;]*);([^;]*);([^;]*);([^;]*);([^;]*);([^;]*);([^;]*);([^;]*);([^;]*);([^;]*);([^;]*);([^;]*);([^;]*)""".r

    /**
     * Regex for detecting a row represnting the beginning of a code point range.
     */
    private val startRegex: Regex =
      """\s*<([^,]+),\s*[fF]irst>\s*""".r

    /**
     * Regex for detecting a row represnting the end of a code point range.
     */
    private val endRegex: Regex =
      """\s*<([^,]+),\s*[lL]ast>\s*""".r

    /**
     * Parse the name and range type from the first field of a row from `UnicodeData.txt`.
     */
    private def parseNameAndRange(value: String): (Option[RangeType], Name) =
      value match {
        case startRegex(name) =>
          Some(RangeType.Start) -> Name(name)
        case endRegex(name) =>
          Some(RangeType.End) -> Name(name)
        case name =>
          None -> Name(name)
      }

    /**
     * Parse a row from `UnicodeData.txt`.
     */
    def unsafeFromLine(value: String): UnicodeDataRow =
      value.trim match {
        case rowRegex(
              codeValue,
              nameAndRange,
              generalCategory,
              canonicalCombiningClass,
              bidirectionalCategory,
              characterDecompositionMapping,
              decimalDigitValue,
              digitValue,
              numericValue,
              mirrored,
              unicode1Name,
              iso10646Comment,
              uppercaseMapping,
              lowercaseMapping,
              titlecaseMapping) =>
          parseNameAndRange(nameAndRange) match {
            case (rangeType, name) =>
              UnicodeDataRow(
                CodePoint.unsafeFromHexString(codeValue),
                rangeType,
                UnicodeCodePointInfomation(
                  name,
                  GeneralCategory(generalCategory),
                  CanonicalCombiningClass.unsafeFromString(canonicalCombiningClass),
                  BidirectionalCategory(bidirectionalCategory),
                  CharacterDecompositionMapping
                    .fromString(characterDecompositionMapping)
                    .toOption,
                  DecimalDigitValue.unsafeFromString(decimalDigitValue),
                  DigitValue.unsafeFromString(digitValue),
                  NumericValue.fromString(numericValue).toOption,
                  Mirrored.unsafeFromString(mirrored),
                  Unicode1Name.fromString(unicode1Name).toOption,
                  ISO10646Comment.fromString(iso10646Comment).toOption,
                  UppercaseMapping.fromString(uppercaseMapping).toOption,
                  LowercaseMapping.fromString(lowercaseMapping).toOption,
                  TitlecaseMapping.fromString(titlecaseMapping).toOption
                )
              )
          }
        case otherwise =>
          throw new IllegalArgumentException(
            s"Expected 15 fields in UnicodeData.txt row, but got ${otherwise.size}, in ${value}")
      }

    implicit val hashAndOrderForUnicodeDataRow
        : Hash[UnicodeDataRow] with Order[UnicodeDataRow] = {
      val order: Order[UnicodeDataRow] = semiauto.order

      new Hash[UnicodeDataRow] with Order[UnicodeDataRow] {
        override def hash(x: UnicodeDataRow): Int = x.hashCode

        override def compare(x: UnicodeDataRow, y: UnicodeDataRow): Int =
          order.compare(x, y)
      }
    }

    implicit def orderingInstance: Ordering[UnicodeDataRow] =
      hashAndOrderForUnicodeDataRow.toOrdering
  }

  /**
   * A representation of data about Unicode code point ranges. The primary feature of this type
   * is the pinning of the keys to a [[CodePointRange]] and the forced grouping of contiguous
   * ranges (e.g. a call to [[#group]]) on access to the underlying `SortedMap`.
   */
  // There are hints of a more general construct here that perhaps should be
  // factored out at some point, but seems a bit niche and would probably be
  // premature optimization.
  final private[build] class UnicodeData[A](
      private val codePoints: SortedMap[CodePointRange, A]
  )(implicit A: Order[A]) {

    lazy val asSortedMap: SortedMap[CodePointRange, A] =
      group(codePoints)

    def mapValues[B: Order](f: A => B): UnicodeData[B] =
      UnicodeData(asSortedMap.mapValues(f))

    def filter(pred: ((CodePointRange, A)) => Boolean): UnicodeData[A] =
      UnicodeData(asSortedMap.filter(pred))

    def updated(k: CodePointRange, v: A): UnicodeData[A] =
      new UnicodeData[A](codePoints.updated(k, v))

    def +(k: CodePointRange, v: A): UnicodeData[A] =
      updated(k, v)

    def +(kv: (CodePointRange, A)): UnicodeData[A] =
      updated(kv._1, kv._2)

    /**
     * The data partitioned into sets which pertain to a single code point and a range of code
     * points. This partitioning is important for optimizing the generated code.
     */
    lazy val partitioned: (SortedMap[CodePointRange.Single, A], SortedMap[CodePointRange, A]) =
      asSortedMap.foldLeft(
        (SortedMap.empty[CodePointRange.Single, A], SortedMap.empty[CodePointRange, A])) {
        case ((singles, ranges), (k: CodePointRange.Single, v)) =>
          (singles + (k -> v), ranges)
        case ((singles, ranges), (k, v)) =>
          (singles, ranges + (k -> v))
      }

    /**
     * As [[#partitioned]], but just returns the keys sets. Useful when we want to filter to a
     * set of code points which have some property, then discard the property information.
     */
    lazy val partitionedKeySets: (SortedSet[CodePointRange.Single], SortedSet[CodePointRange]) =
      partitioned match {
        case (a, b) =>
          (a.keySet, b.keySet)
      }

    final override def toString: String =
      s"UnicodeData(codePoints = ${codePoints})"

    final override def equals(that: Any): Boolean =
      that match {
        case that: UnicodeData[_] =>
          this.asSortedMap.equals(that.asSortedMap)
        case _ =>
          false
      }

    final override def hashCode: Int =
      asSortedMap.hashCode
  }

  private[build] object UnicodeData {
    def apply[A: Order](value: SortedMap[CodePointRange, A]): UnicodeData[A] =
      new UnicodeData[A](value)

    def empty[A: Order]: UnicodeData[A] = UnicodeData(SortedMap.empty[CodePointRange, A])

    implicit def hashAndOrderForUnicodeData[A: Order]
        : Hash[UnicodeData[A]] with Order[UnicodeData[A]] = {
      new Hash[UnicodeData[A]] with Order[UnicodeData[A]] {
        override def hash(x: UnicodeData[A]): Int =
          x.hashCode

        override def compare(x: UnicodeData[A], y: UnicodeData[A]): Int =
          x.asSortedMap.compare(y.asSortedMap)
      }
    }

    implicit def orderingInstance[A: Order]: Ordering[UnicodeData[A]] =
      hashAndOrderForUnicodeData.toOrdering
  }

  /**
   * Convert rows into an instance of [[UnicodeData]]
   *
   * The primary purpose of this function is to deal with the start/end range rows.
   */
  private def rowsToUnicodeData(
      rows: Chain[UnicodeDataRow]): Either[String, UnicodeData[UnicodeCodePointInfomation]] =
    rows.foldMap(row =>
      row
        .rangeType
        .fold(
          (SortedSet(row), SortedMap.empty[Name, NonEmptyChain[UnicodeDataRow]])
        )(_ =>
          (
            SortedSet.empty[UnicodeDataRow],
            SortedMap[Name, NonEmptyChain[UnicodeDataRow]](
              row.unicodeCodePointInformation.name -> NonEmptyChain.one(row))))) match {
      case (singleCodePoints, rangesOfCodePoints) =>
        type F[A] = Either[String, A]
        rangesOfCodePoints
          .toList
          .map(_._2)
          .foldM[F, UnicodeData[UnicodeCodePointInfomation]](
            UnicodeData.empty[UnicodeCodePointInfomation]) {
            case (acc, values) =>
              values.toList match {
                case a :: b :: Nil =>
                  def from(startRow: UnicodeDataRow, endRow: UnicodeDataRow)
                      : Either[String, UnicodeData[UnicodeCodePointInfomation]] =
                    if (startRow.unicodeCodePointInformation === endRow.unicodeCodePointInformation) {
                      CodePointRange
                        .from(startRow.codeValue, endRow.codeValue)
                        .flatMap(range =>
                          Right(
                            acc + (range, startRow.unicodeCodePointInformation)
                          ))
                    } else {
                      Left(
                        s"Code point description is mismatched, but it should be the same for a given range: $a, $b")
                    }

                  (a.rangeType, b.rangeType) match {
                    case (Some(RangeType.Start), Some(RangeType.End)) =>
                      from(a, b)
                    case (Some(RangeType.End), Some(RangeType.Start)) =>
                      from(b, a)
                    case _ =>
                      Left(
                        s"Both range types are the same, but we expected a Start and an End: $a, $b")
                  }
                case otherwise =>
                  Left(
                    s"Expected two rows for a range, a start and an end, but got: ${otherwise}")
              }
          }
          .map(value =>
            singleCodePoints.foldLeft(value) {
              case (acc, value) =>
                acc + (CodePointRange(value.codeValue) -> value.unicodeCodePointInformation)
            })
    }

  /**
   * Extract the subset of Unicode code points which have the General_Category=Mark, e.g. a
   * combining mark.
   */
  private def filterToGeneralCategoryMark(value: UnicodeData[UnicodeCodePointInfomation])
      : (SortedSet[CodePointRange.Single], SortedSet[CodePointRange]) =
    value
      .mapValues(_.generalCategory)
      .filter {
        case (_, v) =>
          v.isCombiningMark
      }
      .partitionedKeySets

  /**
   * Create the term representing the `BitSet` of all the Unicode code points which have a
   * general category of "Mark", e.g. a combining mark.
   */
  private def combiningMarkExpression(
      unicodeData: UnicodeData[UnicodeCodePointInfomation]): Term =
    filterToGeneralCategoryMark(unicodeData) match {
      case (singles, ranges) =>
        val rangeTerms: List[Term] =
          ranges
            .toList
            .map(range =>
              q"Range.inclusive(${Lit.Int(range.lower.value)}, ${Lit.Int(range.upper.value)})")
        val singlesTerm: Term =
          q"BitSet(..${singles.toList.map(value => Lit.Int(value.value.value))})"
        if (rangeTerms.isEmpty) {
          singlesTerm
        } else {
          q"List(..$rangeTerms).foldLeft($singlesTerm){ case (acc, value) => BitSet.fromScalaRange(value) | acc}.compact"
        }
    }

  /**
   * Create the defs needed for the bidirectional information about Unicode code points.
   */
  private def bidirectionalCategoryDefs(
      unicodeData: UnicodeData[UnicodeCodePointInfomation]): List[Defn] = {
    val categoryData: UnicodeData[BidirectionalCategory] =
      unicodeData.mapValues(
        _.bidirectionalCategory
      )
    val (singles, ranges): (
        SortedMap[CodePointRange.Single, BidirectionalCategory],
        SortedMap[CodePointRange, BidirectionalCategory]) = categoryData.partitioned
    val rangeTerms: List[Term] = ranges.toList.map {
      case (k, v) =>
        q"(Range.inclusive(${Lit.Int(k.lower.value)}, ${Lit.Int(k.upper.value)}), ${Lit.String(v.value)})"
    }
    val singleTerms: List[Term] =
      singles.toList.map {
        case (k, v) =>
          q"(${Lit.Int(k.lower.value)}, ${Lit.String(v.value)})"
      }
    val baseMap: Term =
      q"IntMap(..$singleTerms)"

    List(
      q"""private final def bidirectionalCategoryBaseMap: IntMap[String] = $baseMap""",
      q"""override final protected lazy val bidirectionalCategoryMap: IntMap[String] =
             List[(Range, String)](..$rangeTerms).foldLeft(bidirectionalCategoryBaseMap){
              case (k, (range, result)) =>
                range.foldLeft(k){
                  case (k, cp) =>
                    k.updated(cp, result)
                }
          }"""
    )
  }

  /**
   * Extract out the Unicode code points which have a canonical combining class of Virama. This
   * is the only class we need to know about for UTS-46.
   */
  private def filterToVirama(value: UnicodeData[UnicodeCodePointInfomation])
      : (SortedSet[CodePointRange.Single], SortedSet[CodePointRange]) =
    value
      .mapValues(
        _.canonicalCombiningClass
      )
      .filter {
        case (_, v) =>
          v.isVirama
      }
      .partitionedKeySets

  /**
   * Generate the defs needed to create the BitSets for tracking the Unicode code points which
   * have a canonical combining class of Virama.
   */
  private def viramaCanonicalCombiningClassCodePointsDefs(
      unicodeData: UnicodeData[UnicodeCodePointInfomation]): List[Defn] = {
    val (singles, ranges): (SortedSet[CodePointRange.Single], SortedSet[CodePointRange]) =
      filterToVirama(unicodeData)

    val rangeTerms: List[Term] = ranges.toList.map {
      case value =>
        q"(Range.inclusive(${Lit.Int(value.lower.value)}, ${Lit.Int(value.upper.value)}))"
    }

    val singleTerms: List[Term] = singles.toList.map {
      case value =>
        Lit.Int(value.lower.value)
    }

    val base: Term =
      q"BitSet(..$singleTerms)"

    List(
      q"private final def viramaCanonicalCombiningClassCodePointsBase: BitSet = $base",
      q"""override final protected lazy val viramaCanonicalCombiningClassCodePoints: BitSet =
            List[Range](..$rangeTerms).foldLeft(viramaCanonicalCombiningClassCodePointsBase){
              case (acc, range) =>
                range.foldLeft(acc){ case (acc, cp) => acc + cp}
            }
       """
    )
  }

  // Utility functions for working with Maps

  /**
   * For a given `SortedMap`, invert the mapping.
   */
  private def invertMapping[A: Order, B: Ordering: Order](
      value: SortedMap[A, B]): SortedMap[B, NonEmptySet[A]] =
    value.toList.foldMap {
      case (k, v) =>
        SortedMap(v -> NonEmptySet.one(k))
    }

  /**
   * For a map of code point range values to some type `A`, collapse ranges of code points which
   * map to the same `A` into a single code point range. This becomes useful when we are
   * interested in a certain sub-property of a code point, for example the "General_Category".
   * When initially parsing the data from `UnicodeData.txt`, these code points might have been
   * in separate rows because they didn't share all of the code point properties, but after
   * projecting to a specific sub-property they may be collapsable into a single range.
   *
   * By grouping these values when we can, we make the size of the generated code smaller.
   */
  private def group[A: Eq: Order](
      value: SortedMap[CodePointRange, A]): SortedMap[CodePointRange, A] = {
    implicit def ordering: Ordering[A] = Order[A].toOrdering
    invertMapping(value).foldLeft(SortedMap.empty[CodePointRange, A]) {
      // Remember we just inverted this.
      case (acc, (value, keys)) =>
        keys
          .foldLeft((acc, Option.empty[CodePointRange])) {
            case ((acc, previous), key) =>
              previous.fold(
                (acc + (key -> value), Some(key))
              )(previous =>
                if (previous.upper.value < Character.MAX_CODE_POINT && (previous
                    .upper
                    .value + 1) === key.lower.value) {
                  val newRange: CodePointRange =
                    CodePointRange.unsafeFrom(previous.lower, key.upper)
                  ((acc - previous) + (newRange -> value), Some(newRange))
                } else {
                  (acc + (key -> value), Some(key))
                })
          }
          ._1
    }
  }

  /**
   * Missing in 2.12.x, deprecated in 2.13.x, so reproduced here.
   */
  private def mapValues[A: Ordering, B, C](value: SortedMap[A, B], f: B => C): SortedMap[A, C] =
    value.foldLeft(SortedMap.empty[A, C]) {
      case (acc, (k, v)) =>
        acc + (k -> f(v))
    }
}
