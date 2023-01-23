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
import cats.syntax.all._
import java.net.URI
import java.net.URL
import sbt.{Show => _, _}
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet
import scala.meta._
import scala.util.matching.Regex

/**
 * Code generation utilities for code which is derived from `DerivedJoiningType.txt`.
 *
 * It is intended that the `generate` method will be invoked from `CodeGen`. This is important
 * because we generate code from several different sources and we need to ensure that the
 * UnicodeVersion used is always the same.
 */
private[build] object DerivedJoiningTypeCodeGen {

  final private val BaseName = "JoiningType"
  final private val BaseTypeName: String = s"${BaseName}Base"
  final private val GeneratedTypeName: String = s"Generated${BaseName}"

  /**
   * Generate the file for the functions, methods, and data derived from
   * `DerivedJoiningType.txt`.
   */
  def generate(
      dir: File,
      unicodeVersion: UnicodeVersion
  ): Either[Throwable, File] = {
    val outputFile: File =
      dir / "org" / "typelevel" / "idna4s" / "core" / "uts46" / s"${GeneratedTypeName}.scala"

    rowsFromUnicodeVersion(unicodeVersion)
      .flatMap(rows => joiningTypeMap(rows).leftMap(e => new RuntimeException(e)))
      .map(generateSource)
      .flatMap(tree =>
        Either.catchNonFatal(IO.write(outputFile, tree.syntax)).map(_ => outputFile))
  }

  /**
   * For some lines, attempt to parse them into [[Row]] values.
   */
  def rowsFromLines[F[_]: Foldable](
      lines: F[String]): Either[NonEmptyChain[String], SortedSet[Row]] =
    lines.parFoldMapA {
      case line =>
        if (line.trim.isEmpty || line.trim.startsWith("#")) {
          Right(SortedSet.empty[Row])
        } else {
          Row.fromLine(line).map(row => SortedSet(row))
        }
    }

  /**
   * Attempt to parse [[Row]] values from a `URL` which represents a format compatible with
   * `DerivedJoiningType.txt`.
   */
  def rowsFromUrl(url: URL): Either[Throwable, SortedSet[Row]] =
    Either
      .catchNonFatal(
        IO.readLinesURL(url)
      )
      .flatMap(lines =>
        rowsFromLines(lines).leftMap(errors =>
          new RuntimeException(s"""Error(s) parsing rows: ${errors.mkString_(", ")}""")))

  /**
   * Attempt to parse [[Row]] values by downloading `DerivedJoiningType.txt` for the given
   * Unicode version.
   */
  def rowsFromUnicodeVersion(
      unicodeVersion: UnicodeVersion): Either[Throwable, SortedSet[Row]] =
    Either
      .catchNonFatal(
        URI
          .create(
            s"https://www.unicode.org/Public/${unicodeVersion.asString}/ucd/extracted/DerivedJoiningType.txt")
          .toURL()
      )
      .flatMap(url => rowsFromUrl(url))

  /**
   * Attempt to convert some [[Row]] values into a mapping between [[CodePointRange]] values and
   * [[JoiningType]] values.
   */
  def joiningTypeMap[F[_]: Foldable](
      rows: F[Row]): Either[String, SortedMap[CodePointRange, JoiningType]] =
    flattenValues(
      rows.foldMap {
        case Row(codePointRange, joiningType) =>
          SortedMap(codePointRange -> NonEmptyChain.one(joiningType))
      }
    )

  /**
   * Create the source code for the generated file.
   */
  def generateSource(value: SortedMap[CodePointRange, JoiningType]): Tree =
    source"""
package org.typelevel.idna4s.core.uts46

import scala.collection.immutable.IntMap

private[uts46] trait ${Type.Name(GeneratedTypeName)} extends ${Init(
        Type.Name(BaseTypeName),
        scala.meta.Name(""),
        Seq.empty)} {
..${joiningTypeMapDef(value)}
}
"""

  /**
   * Create the Defn values for the [[CodePointRange]] to [[JoiningType]] mappings. These Defn
   * values will implement the interface defined in `JoiningTypeBase`.
   */
  def joiningTypeMapDef(value: SortedMap[CodePointRange, JoiningType]): List[Defn] = {
    val (singles, ranges): (
        SortedMap[CodePointRange.Single, JoiningType],
        SortedMap[CodePointRange, JoiningType]) =
      value
        .toVector
        .foldLeft(
          (
            SortedMap.empty[CodePointRange.Single, JoiningType],
            SortedMap.empty[CodePointRange, JoiningType])) {
          case ((singles, ranges), (cp: CodePointRange.Single, joiningType)) =>
            (singles + (cp -> joiningType), ranges)
          case ((singles, ranges), (cp, joiningType)) =>
            (singles, ranges + (cp -> joiningType))
        }

    val rangeTerms: List[Term] =
      ranges.toList.map {
        case (k, v) =>
          q"(Range.inclusive(${Lit.Int(k.lower.value)}, ${Lit.Int(k.upper.value)}), ${Lit.Char(v.value)})"
      }

    val singleTerms: List[Term] =
      singles.toList.map {
        case (k, v) =>
          q"(${Lit.Int(k.value.value)}, ${Lit.Char(v.value)})"
      }

    def baseMap: Term =
      q"IntMap(..$singleTerms)"

    val rhs: Term =
      if (singleTerms.isEmpty && rangeTerms.isEmpty) {
        q"IntMap.empty[Char]"
      } else if (rangeTerms.isEmpty) {
        baseMap
      } else {
        q"List(..$rangeTerms).foldLeft($baseMap){case (acc, (range, value)) => range.foldLeft(acc){case (acc, cp) => acc.updated(cp, value)}}"
      }

    List(
      q"""override final protected lazy val joiningTypeMap: IntMap[Char] = $rhs"""
    )
  }

  /**
   * Newtype for a JoiningType, which Unicode represents as a single character value.
   */
  final class JoiningType private (val value: Char) extends AnyVal {
    override def toString: String =
      s"JoiningType(value = ${value})"
  }

  object JoiningType {

    /**
     * Attempt to create a [[JoiningType]] value from a char value.
     */
    def fromChar(value: Char): Either[String, JoiningType] =
      if (value.isLetter) {
        Right(new JoiningType(value))
      } else {
        Left(s"Only letter values are defined to reprsent Unicode JoiningType values.")
      }

    def unsafeFromChar(value: Char): JoiningType =
      fromChar(value).fold(
        e => throw new IllegalArgumentException(e),
        identity
      )

    /**
     * Attempt to create a [[JoiningType]] value from a `String` which should contain a single
     * char value.
     */
    def fromCharString(value: String): Either[String, JoiningType] =
      if (value.size === 1) {
        fromChar(value.head)
      } else {
        Left(
          s"fromCharString expects a String with a single character, but got a String of size: ${value.size}")
      }

    implicit val hashAndOrderForJoiningType: Hash[JoiningType] with Order[JoiningType] =
      new Hash[JoiningType] with Order[JoiningType] {
        override def hash(x: JoiningType): Int = x.hashCode

        override def compare(x: JoiningType, y: JoiningType): Int =
          x.value.compare(y.value)
      }

    implicit def orderingInstance: Ordering[JoiningType] =
      hashAndOrderForJoiningType.toOrdering

    implicit val showForJoiningType: Show[JoiningType] =
      Show.fromToString
  }

  /**
   * A reprsentation of a row in `DerivedJoiningType.txt`.
   */
  final case class Row(codePointRange: CodePointRange, joiningType: JoiningType)

  object Row {
    private val codePointRangeRowRegex: Regex =
      """\s*([0-9a-fA-F]+)\.\.([0-9a-fA-F]+)\s*;\s*([A-Za-z]).*""".r

    private val codePointRowRegex: Regex =
      """\s*([0-9a-fA-F]+)\s*;\s*([A-Za-z]).*""".r

    /**
     * Attempt to parse a [[Row]] from a `String` which should represent a line in
     * `DerivedJoiningType.txt`.
     */
    def fromLine(value: String): Either[NonEmptyChain[String], Row] =
      value match {
        case codePointRangeRowRegex(lower, upper, joiningType) =>
          (
            CodePointRange.fromHexStrings(lower, upper).leftMap(NonEmptyChain.one),
            JoiningType.fromCharString(joiningType).leftMap(NonEmptyChain.one)).parMapN {
            case (codePointRange, joiningType) =>
              Row(codePointRange, joiningType)
          }
        case codePointRowRegex(codePoint, joiningType) =>
          (
            CodePointRange.fromHexString(codePoint).leftMap(NonEmptyChain.one),
            JoiningType.fromCharString(joiningType).leftMap(NonEmptyChain.one)).parMapN {
            case (codePointRange, joiningType) =>
              Row(codePointRange, joiningType)
          }
        case otherwise =>
          Left(
            NonEmptyChain(
              s"Unable to parse line as a DerivedJoiningType.txt row: ${otherwise}"))
      }

    implicit val hashAndOrderForRow: Hash[Row] with Order[Row] =
      new Hash[Row] with Order[Row] {
        override def hash(x: Row): Int =
          x.hashCode

        override def compare(x: Row, y: Row): Int =
          x.codePointRange.compare(y.codePointRange) match {
            case 0 =>
              x.joiningType.compare(y.joiningType)
            case otherwise =>
              otherwise
          }
      }

    implicit def orderingInstance: Ordering[Row] =
      hashAndOrderForRow.toOrdering
  }

  /**
   * Utility method to flatten a map of A -> F[B], where F is some non-empty type such as
   * `NonEmptyChain` into a mapping of A -> B. This will fail if there is more than one `B` for
   * a given `A`.
   */
  def flattenValues[F[_]: Reducible, A: Ordering: Show, B: Eq: Show](
      fa: SortedMap[A, F[B]]): Either[String, SortedMap[A, B]] =
    fa.toVector.foldM(SortedMap.empty[A, B]) {
      case (acc, (k, values)) =>
        values
          .reduceLeftM(value => Right(value): Either[String, B]) {
            case (firstValue, value) =>
              if (firstValue === value) {
                Right(firstValue)
              } else {
                Left(
                  show"More than one distinct mapping for distinct key $k: ${firstValue}, ${value}")
              }
          }
          .map(value => acc + (k -> value))
    }
}
