/*
 * Copyright 2023 Typelevel
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
import cats.syntax.all._
import java.net.URI
import java.net.URL
import sbt.{Show => _, _}
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet
import scala.util.matching._
import scala.meta._

/**
 * Code generation utilities for code which is derived from `DerivedBidiClass.txt`.
 *
 * It is intended that the `generate` method will be invoked from `CodeGen`. This is important
 * because we generate code from several different sources and we need to ensure that the
 * UnicodeVersion used is always the same.
 */
private[build] object DerivedBidiClassCodeGen {

  final private val BaseName = "BidirectionalClass"
  final private val BaseTypeName: String = s"${BaseName}Base"
  final private val GeneratedTypeName: String = s"Generated${BaseName}"

  /**
   * Generate the file for the functions, methods, and data derived from `DerivedBidiClass.txt`.
   */
  def generate(dir: File, unicodeVersion: UnicodeVersion): Either[Throwable, File] = {
    def outputFile: File =
      dir / "org" / "typelevel" / "idna4s" / "core" / "uts46" / s"${GeneratedTypeName}.scala"

    bidiMappingFromUnicodeVersion(unicodeVersion)
      .flatMap(mapping =>
        generateSource(mapping).leftMap(errors =>
          s"""Unable to generate source file due to ${errors.mkString_(", ")}"""))
      .leftMap(e => new RuntimeException(e)) flatMap (tree =>
      Either.catchNonFatal(IO.write(outputFile, tree.syntax)).as(outputFile))
  }

  /**
   * Newtype for bidirectional alias values.
   *
   * For example, for the bidirectional class named "Left_To_Right", the `BidiAlias` string is
   * "L".
   */
  final case class BidiAlias(value: String) extends AnyVal

  object BidiAlias {

    implicit val hashAndOrderForBidiAlias: Hash[BidiAlias] with Order[BidiAlias] =
      new Hash[BidiAlias] with Order[BidiAlias] {
        override def hash(x: BidiAlias): Int =
          x.hashCode

        override def compare(x: BidiAlias, y: BidiAlias): Int =
          x.value.compare(y.value)
      }

    implicit def orderingInstance: Ordering[BidiAlias] = hashAndOrderForBidiAlias.toOrdering

    implicit val showForBidiAlias: Show[BidiAlias] =
      Show.fromToString
  }

  /**
   * Newtype for bidirectional name values.
   */
  final case class BidiName(value: String) extends AnyVal

  object BidiName {
    implicit val hashAndOrderForBidiName: Hash[BidiName] with Order[BidiName] =
      new Hash[BidiName] with Order[BidiName] {
        override def hash(x: BidiName): Int =
          x.hashCode

        override def compare(x: BidiName, y: BidiName): Int =
          x.value.compare(y.value)
      }

    implicit def orderingInstance: Ordering[BidiName] = hashAndOrderForBidiName.toOrdering

    implicit val showForBidiName: Show[BidiName] =
      Show.fromToString
  }

  /**
   * Regular expression used to extract the bidi alias when parsing the bidi name to bidi alias
   * mapping.
   */
  private val bidiPropertyValueAliasRegex: Regex =
    """\s*bc\s*;\s*(\w+)\s*;\s*(\w+).*""".r

  /**
   * Create the [[BidiName]] to [[BidiAlias]] mapping for the given version of Unicode.
   */
  def bidiPropertyValueAlisesForUnicodeVersion(
      unicodeVersion: UnicodeVersion): Either[String, SortedMap[BidiName, BidiAlias]] =
    Either
      .catchNonFatal(
        URI
          .create(
            s"https://www.unicode.org/Public/${unicodeVersion.asString}/ucd/PropertyValueAliases.txt")
          .toURL()
      )
      .leftMap(_.getLocalizedMessage)
      .flatMap(
        bidiPropertyValueAlisesFromURL
      )

  /**
   * Create the [[BidiName]] to [[BidiAlias]] mapping for the given URL which should point to a
   * version of the Unicode character data file named "PropertyValueAliases.txt".
   */
  def bidiPropertyValueAlisesFromURL(url: URL): Either[String, SortedMap[BidiName, BidiAlias]] =
    Either
      .catchNonFatal(
        IO.readLinesURL(url)
      )
      .leftMap(_.getLocalizedMessage)
      .flatMap((lines: List[String]) => bidiPropertyValueAliasesFromLines(lines))

  /**
   * Create the [[BidiName]] to [[BidiAlias]] mapping for some `Foldable` set of lines. These
   * lines should represent some version of the Unicode character data file
   * "PropertyValueAliases.txt".
   */
  def bidiPropertyValueAliasesFromLines[F[_]: Foldable](
      lines: F[String]): Either[String, SortedMap[BidiName, BidiAlias]] =
    flattenValuesOrError(
      lines.foldMap {
        case bidiPropertyValueAliasRegex(alias, name) =>
          SortedMap(BidiName(name) -> NonEmptySet.one(BidiAlias(alias)))
        case _ =>
          SortedMap.empty[BidiName, NonEmptySet[BidiAlias]]
      }
    )

  /**
   * Create the mapping between [[CodePointRange]] values for a given version of Unicode.
   *
   * @note
   *   This method will download ''two'' files. Some of the bidirectional mappings are only
   *   defined in terms of the `@missing` annotated comments. These comments describe the
   *   mapping in terms for the [[BidiName]], however the normal body of the
   *   "DerivedBidiClass.txt" file uses the [[BidiAlias]]. For this reason, we have to download
   *   the mapping from [[BidiName]] to [[BidiAlias]] so that we can map the `@missing` values
   *   to [[BidiAlias]] values.
   *
   * @see
   *   [[https://www.unicode.org/reports/tr44/#Missing_Conventions]]
   */
  def bidiMappingFromUnicodeVersion(
      unicodeVersion: UnicodeVersion): Either[String, SortedMap[CodePointRange, BidiAlias]] =
    Either
      .catchNonFatal(
        URI
          .create(
            s"https://www.unicode.org/Public/${unicodeVersion.asString}/ucd/extracted/DerivedBidiClass.txt")
          .toURL()
      )
      .leftMap(_.getLocalizedMessage)
      .flatMap(url =>
        bidiPropertyValueAlisesForUnicodeVersion(unicodeVersion).flatMap(nameToAlias =>
          bidiMappingFromURL(url, nameToAlias)))

  /**
   * Create the mapping between [[CodePointRange]] values for a given URL which should point to
   * a version of the Unicode character data file "DerivedBidiClass.txt".
   *
   * In order to handle the `@missing` annotated classes, a mapping between [[BidiName]] and
   * [[BidiAlias]] values is also required.
   *
   * @see
   *   [[https://www.unicode.org/reports/tr44/#Missing_Conventions]]
   */
  def bidiMappingFromURL(url: URL, nameToAlias: Map[BidiName, BidiAlias])
      : Either[String, SortedMap[CodePointRange, BidiAlias]] =
    Either
      .catchNonFatal(
        IO.readLinesURL(url)
      )
      .leftMap(_.getLocalizedMessage)
      .flatMap((lines: List[String]) => bidiMappingFromLines(lines, nameToAlias))

  /**
   * Create the mapping between [[CodePointRange]] values for a `Foldable` of lines which should
   * represent a version of the Unicode character data file "DerivedBidiClass.txt".
   *
   * In order to handle the `@missing` annotated classes, a mapping between [[BidiName]] and
   * [[BidiAlias]] values is also required.
   *
   * @see
   *   [[https://www.unicode.org/reports/tr44/#Missing_Conventions]]
   */
  def bidiMappingFromLines[F[_]: Foldable](fa: F[String], nameToAlias: Map[BidiName, BidiAlias])
      : Either[String, SortedMap[CodePointRange, BidiAlias]] =
    fa.foldM(
      (Chain.empty[(CodePointRange, BidiName)], Chain.empty[(CodePointRange, BidiAlias)])) {
      case (acc, line) if line.trim.isEmpty =>
        // Skip empty lines
        Right(acc)
      case (acc @ (missingLines, normalLines), line) =>
        if (line.trim.startsWith("#")) {
          Right(
            parseMissingBidiClassLine(line).fold(
              // Left just means it is a comment
              _ => acc,
              value => (missingLines :+ value, normalLines)
            )
          )
        } else {
          parseBidiClassLine(line).fold(
            e => Left(e),
            {
              case (k, v) =>
                Right(
                  (missingLines, normalLines :+ (k -> v))
                )
            }
          )
        }
    }.flatMap {
      case (missingLines, normalLines) =>
        missingLines
          .foldM(Chain.empty[(CodePointRange, BidiAlias)]) {
            case (acc, (k, v)) =>
              nameToAlias
                .get(v)
                .fold(
                  Left(s"Missing Bidi name to alias mapping for $v."): Either[
                    String,
                    Chain[(CodePointRange, BidiAlias)]]
                )(alias => Right(acc :+ (k -> alias)))
          }
          .map(missingLines => (missingLines, normalLines))
    }.map {
      case (missingLines, normalLines) =>
        // We have to merge the missing lines with normal lines. Order is
        // important here, because overlapping @missing lines defined later in
        // DerivedBidiClass.txt take precedence over mappings define earlier
        // and mappings in the normal body of DerivedBidiClass.txt take
        // precedence over all @missing mappings.
        //
        // O(n^2), we need an interval tree to make this better (which we
        // don't have in Scala yet), but it is compile time only and the value
        // is small enough that compile times aren't noticeably bad.
        CodePointRange.resolveMissingMapping(missingLines ++ normalLines)
    }.flatMap { bidiMapping =>
      // O(n^2) sanity check. Again, can be made better with an Interval tree,
      // but fast enough that it's okay for now.

      implicit def ordering: Ordering[NonEmptySet[CodePointRange]] =
        Order[NonEmptySet[CodePointRange]].toOrdering

      val overlapping: SortedSet[NonEmptySet[CodePointRange]] =
        bidiMapping.toVector.foldMap {
          case (a, _) =>
            NonEmptySet
              .fromSet(
                bidiMapping.toVector.foldMap {
                  case (b, _) =>
                    if (a =!= b && a.overlapsWith(b)) {
                      SortedSet(b)
                    } else {
                      SortedSet.empty[CodePointRange]
                    }
                }
              )
              .fold(
                SortedSet.empty[NonEmptySet[CodePointRange]]
              )(nes => SortedSet(nes))
        }

      if (overlapping.isEmpty) {
        Right(bidiMapping)
      } else {
        Left(
          s"""Overlapping BIDI ranges found, this is a bug: ${overlapping.mkString(", \n")}""")
      }
    }

  /**
   * Regex used to parse an @missing line for a range of code points.
   *
   * {{{
   * # @missing: 0000..10FFFF; Left_To_Right
   * }}}
   */
  private val bidiMissingLineRangeRegex: Regex =
    """\s*#\s*@missing:\s*(\p{XDigit}{4,})\.\.(\p{XDigit}{4,})\s*;\s*(\S+).*""".r

  /**
   * Regex used to parse an @missing line for a single code point. Note, Unicode 15.0 doesn't
   * have any instances of this but it is possible future Unicode revisions will.
   */
  private val bidiMissingLineSingleRegex: Regex =
    """\s*#\s*@missing:\s*(\p{XDigit}{4,})\s*;\s*(\S+).*""".r

  /**
   * For a line which is assumed to be a comment indicating an @missing annotation, attempt to
   * parse it as a [[CodePointRange]] and a [[BidiName]].
   */
  def parseMissingBidiClassLine(line: String): Either[String, (CodePointRange, BidiName)] =
    line match {
      case bidiMissingLineRangeRegex(lower, upper, name) =>
        CodePointRange.fromHexStrings(lower, upper).map(range => range -> BidiName(name))
      case bidiMissingLineSingleRegex(value, name) =>
        CodePointRange.fromHexString(value).map(range => range -> BidiName(name))
      case _ =>
        Left(s"Given input is not a valid @missing bidi declaration: ${line}")
    }

  /**
   * Regex used to parse a bidi mapping from "DerivedBidiClass.txt" for a range of code points.
   *
   * {{{
   * 0041..005A    ; L # L&  [26] LATIN CAPITAL LETTER A..LATIN CAPITAL LETTER Z
   * }}}
   */
  private val bidiLineRangeRegex: Regex =
    """\s*(\p{XDigit}{4,})\.\.(\p{XDigit}{4,})\s*;\s*(\w+).*""".r

  /**
   * Regex used to parse a bidi mapping from "DerivedBidiClass.txt" for a single code point.
   *
   * {{{
   * 00B5          ; L # L&       MICRO SIGN
   * }}}
   */
  private val bidiLineSingleRegex: Regex =
    """\s*(\p{XDigit}{4,})\s*;\s*(\w+).*""".r

  /**
   * For a line which is assumed to be a bidi mapping line, attempt to parse it as a
   * [[CodePointRange]] and [[BidiAlias]].
   */
  def parseBidiClassLine(line: String): Either[String, (CodePointRange, BidiAlias)] =
    line match {
      case bidiLineRangeRegex(lower, upper, bidiAlias) =>
        CodePointRange.fromHexStrings(lower, upper).map(range => range -> BidiAlias(bidiAlias))
      case bidiLineSingleRegex(value, bidiAlias) =>
        CodePointRange.fromHexString(value).map(range => range -> BidiAlias(bidiAlias))
      case _ =>
        Left(s"Given input is not a valid bidi class declaration: ${line}")
    }

  /**
   * Attempt to generate the `Defn` values for the members of `GeneratedBidirectionalClass`.
   */
  def bidiClassMappingDef(value: SortedMap[CodePointRange, BidiAlias])
      : Either[NonEmptyChain[String], List[Defn]] = {
    value.toVector.foldMap {
      case (single: CodePointRange.Single, value) =>
        (
          SortedMap(single -> NonEmptyChain.one(value)),
          SortedMap.empty[CodePointRange, NonEmptyChain[BidiAlias]])
      case (range, value) =>
        (
          SortedMap.empty[CodePointRange.Single, NonEmptyChain[BidiAlias]],
          SortedMap(range -> NonEmptyChain(value)))
    } match {
      case (singles, ranges) =>
        (
          flattenValuesOrError(singles).leftMap(NonEmptyChain.one),
          flattenValuesOrError(ranges).leftMap(NonEmptyChain.one)).parMapN {
          case (singles, ranges) =>
            def rangeTerms: List[Term] =
              ranges.toList.map {
                case (k, v) =>
                  q"(Range.inclusive(${Lit.Int(k.lower.value)}, ${Lit.Int(
                      k.upper.value)}), ${Lit.String(v.value)})"
              }

            def singleTerms: List[Term] =
              singles.toList.map {
                case (k, v) =>
                  q"(${Lit.Int(k.value.value)}, ${Lit.String(v.value)})"
              }

            def baseMap: Term =
              q"IntMap(..$singleTerms)"

            List(
              q"private[this] final def bidiSingleCodePoints = $baseMap",
              q"private[this] final lazy val bidiMap = List(..$rangeTerms).foldLeft(bidiSingleCodePoints){case (acc, (range, value)) => range.foldLeft(acc){case (acc, cp) => acc.updated(cp, value)}}",
              q"""override final protected def bidiTypeForCodePointInt(cp: Int): String = bidiMap(cp)"""
            )
        }
    }
  }

  /**
   * Attempt to generate the source file tree for "GeneratedBidirectionalClass.scala"
   */
  def generateSource(
      value: SortedMap[CodePointRange, BidiAlias]): Either[NonEmptyChain[String], Tree] =
    bidiClassMappingDef(value).map((defns: List[Defn]) =>
      source"""
package org.typelevel.idna4s.core.uts46

import scala.collection.immutable.IntMap

private[idna4s] trait ${Type.Name(GeneratedTypeName)} extends ${Init(
          Type.Name(BaseTypeName),
          scala.meta.Name(""),
          Nil)} {

..${defns}
}
""")
}
