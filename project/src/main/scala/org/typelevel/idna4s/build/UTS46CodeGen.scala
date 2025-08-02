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
import java.io.File
import java.net.URI
import sbt._
import scala.annotation.tailrec
import scala.util.matching._
import scala.collection.immutable.SortedSet
import scala.collection.immutable.SortedMap
import scala.meta._

/**
 * Functions for generating the UTS-46 lookup tables for step 1 of UTS-46 processing.
 *
 * @see
 *   [[https://www.unicode.org/reports/tr46/#Processing UTS-46 Section 4]]
 * @see
 *   [[https://www.unicode.org/reports/tr46/#IDNA_Mapping_Table Section 5]]
 */
object UTS46CodeGen {

  /**
   * Download the UTS-46 lookup table sources and generate the UTS-46 lookup table code. This
   * will use the "latest" release from Unicode.
   *
   * @param dir
   *   The base directory that the generated file will be in.
   */
  private[build] def generate(
      dir: File,
      unicodeVersion: UnicodeVersion): Either[Throwable, File] =
    Rows
      .fromUnicodeURL(unicodeVersion)
      .map(
        generateFromRows(_, dir)
      )

  /**
   * Generate the UTS-46 lookup table code.
   *
   * @param rows
   *   The parsed rows representing the source UTS-46 lookup tables for step 1 of processing.
   *
   * @param dir
   *   The base directory that the generated file will be in.
   */
  private def generateFromRows(rows: Rows, dir: File): File = {
    val outputFile: File =
      dir / "org" / "typelevel" / "idna4s" / "core" / "uts46" / "GeneratedCodePointMapper.scala"

    IO.write(outputFile, rows.asSourceFile)

    outputFile
  }

  // Orphan instances for Ordering of NonEmptyList and List

  implicit private def orphanOrderingForNonEmptyList[A: Order]: Ordering[NonEmptyList[A]] =
    Order[NonEmptyList[A]].toOrdering

  implicit private def orphanOrderingForList[A: Order]: Ordering[List[A]] =
    Order[List[A]].toOrdering

  /**
   * ADT for the IDNA 2008 status associated with some UTS-46 valid code points.
   */
  sealed abstract private[build] class IDNA2008Status extends Serializable {
    import IDNA2008Status._

    final def asString: String =
      this match {
        case NV8 => "NV8"
        case XV8 => "XV8"
      }
  }

  private[build] object IDNA2008Status {

    /**
     * Valid under UTS-46, but excluded from all domain names under IDNA 2008 for all versions
     * of Unicode.
     */
    case object NV8 extends IDNA2008Status

    /**
     * Valid under UTS-46, but excluded from all domain names under IDNA 2008 for the
     * ''current'' version of Unicode.
     */
    case object XV8 extends IDNA2008Status

    implicit val orderInstance: Order[IDNA2008Status] =
      new Order[IDNA2008Status] {
        override def compare(x: IDNA2008Status, y: IDNA2008Status): Int =
          (x, y) match {
            case (NV8, XV8) =>
              -1
            case (XV8, NV8) =>
              1
            case _ =>
              0
          }
      }

    implicit def orderingInstance: Ordering[IDNA2008Status] =
      orderInstance.toOrdering

    def fromString(value: String): Either[String, IDNA2008Status] =
      value.trim match {
        case "NV8" => Right(NV8)
        case "XV8" => Right(XV8)
        case _ => Left(s"Unknown IDNA 2008 status string: ${value}")
      }
  }

  /**
   * ADT for parsing the status of a code point, with it's associated data.
   *
   * This represents a given row in the unparsed lookup table, without the input code points and
   * the comment.
   */
  sealed abstract private class CodePointStatus extends Serializable

  private object CodePointStatus {
    implicit val orderInstance: Order[CodePointStatus] =
      new Order[CodePointStatus] {
        override def compare(x: CodePointStatus, y: CodePointStatus): Int =
          (x, y) match {
            case (Valid(x), Valid(y)) =>
              x.compare(y)
            case (Ignored, Ignored) =>
              0
            case (Mapped(x), Mapped(y)) =>
              x.compare(y)
            case (Deviation(x), Deviation(y)) =>
              x.compare(y)
            case (Disallowed, Disallowed) =>
              0
            case (Disallowed_STD3_Valid, Disallowed_STD3_Valid) =>
              0
            case (Disallowed_STD3_Mapped(x), Disallowed_STD3_Mapped(y)) =>
              x.compare(y)
            case (_: Valid, _) =>
              -1
            case (_, _: Valid) =>
              1
            case (Ignored, _) =>
              -1
            case (_, Ignored) =>
              1
            case (_: Mapped, _) =>
              -1
            case (_, _: Mapped) =>
              1
            case (_: Deviation, _) =>
              -1
            case (_, _: Deviation) =>
              1
            case (Disallowed, _) =>
              -1
            case (_, Disallowed) =>
              1
            case (Disallowed_STD3_Valid, _) =>
              -1
            case (_, Disallowed_STD3_Valid) =>
              1
          }

        implicit def orderingInstance: Ordering[CodePointStatus] =
          orderInstance.toOrdering
      }

    /**
     * A valid code point, e.g. it maps to itself under UTS-46.
     *
     * Some code points which are valid under
     */
    final case class Valid(idna2008Status: Option[IDNA2008Status]) extends CodePointStatus

    /**
     * A code point which is ignored under UTS-46, this means it is dropped from the input
     * string.
     */
    case object Ignored extends CodePointStatus

    /**
     * A code point which is mapped to one or more other code points.
     */
    final case class Mapped(mapping: NonEmptyList[CodePoint]) extends CodePointStatus

    /**
     * A code point which represents a "deviation". Deviation code points can, under certain
     * circumstances, yield different IDNs under IDNA 2003 vs. IDNA 2008.
     *
     * They can map to zero or more code points.
     *
     * @see
     *   [[https://www.unicode.org/reports/tr46/#Deviations Deviations]]
     */
    final case class Deviation(mapping: List[CodePoint]) extends CodePointStatus

    /**
     * Code points which are disallowed under UTS-46. Attempting to map a string containing a
     * disallowed code point will yield an error.
     */
    case object Disallowed extends CodePointStatus

    /**
     * Code points which are disallowed unless the UseSTD3ASCIIRules configuration option is
     * false (not recommended). When UseSTD3ASCIIRules is false, these code points are valid,
     * e.g. they map to themselves.
     *
     * @see
     *   [[https://www.unicode.org/reports/tr46/#UseSTD3ASCIIRules UseSTD3ASCIIRules]]
     */
    case object Disallowed_STD3_Valid extends CodePointStatus

    /**
     * Code points which are disallowed unless the UseSTD3ASCIIRules configuration option is
     * false (not recommended). When UseSTD3ASCIIRules is false, these code points are mapped to
     * 1 or more code points.
     *
     * @see
     *   [[https://www.unicode.org/reports/tr46/#UseSTD3ASCIIRules UseSTD3ASCIIRules]]
     */
    final case class Disallowed_STD3_Mapped(mapping: NonEmptyList[CodePoint])
        extends CodePointStatus

    /**
     * Attempt to create a [[CodePointStatus]] value from the status string, the mapping value,
     * and the IDNA 2008 status value.
     */
    def from(
        value: String,
        mapping: MappingValue,
        idna2008Status: Option[IDNA2008Status]): Either[String, CodePointStatus] =
      value.trim match {
        case "valid" if mapping.value.isEmpty =>
          Right(Valid(idna2008Status))
        case "ignored" if idna2008Status.isEmpty && mapping.value.isEmpty =>
          Right(Ignored)
        case "mapped" if idna2008Status.isEmpty =>
          NonEmptyList
            .fromList(mapping.value)
            .fold(
              Left("Mapped status must have at least one output code point."): Either[
                String,
                CodePointStatus]
            )(nel => Right(Mapped(nel)))
        case "deviation" if idna2008Status.isEmpty =>
          Right(Deviation(mapping.value))
        case "disallowed" if idna2008Status.isEmpty && mapping.value.isEmpty =>
          Right(Disallowed)
        case "disallowed_STD3_valid" if idna2008Status.isEmpty && mapping.value.isEmpty =>
          Right(Disallowed_STD3_Valid)
        case "disallowed_STD3_mapped" if idna2008Status.isEmpty =>
          NonEmptyList
            .fromList(mapping.value)
            .fold(
              Left(
                "Disallowed_STD3_Mapped status must have at least one output code point."): Either[
                String,
                CodePointStatus]
            )(nel => Right(Disallowed_STD3_Mapped(nel)))
        case _ =>
          Left(
            s"Unknown or invalid row: Status=${value}, mapping=${mapping}, idna2008Status=${idna2008Status}")
      }
  }

  /**
   * Newtype for an output mapping of code points.
   */
  final private case class MappingValue(value: List[CodePoint])

  private object MappingValue {
    def fromString(value: String): Either[String, MappingValue] = {
      value.trim match {
        case value if value.isEmpty =>
          // Special case for when there is an IDNA 2008 status, but no
          // mapping value.
          Right(empty): Either[String, MappingValue]
        case value =>
          value
            .split(" ")
            .toList
            .foldLeft(Right(List.empty[CodePoint]): Either[String, List[CodePoint]]) {
              case (acc, value) =>
                acc.flatMap(acc => CodePoint.fromInt(Integer.parseInt(value, 16)).map(_ :: acc))
            }
            .map(_.reverse)
            .map(MappingValue.apply)
      }
    }

    val empty: MappingValue = MappingValue(Nil)
  }

  /**
   * A type representing a single row parsed from the UTS-46 lookup tables.
   */
  final private case class Row(
      codePointRange: CodePointRange,
      codePointStatus: CodePointStatus
  )

  private object Row {
    implicit val orderInstance: Order[Row] =
      new Order[Row] {
        override def compare(x: Row, y: Row): Int =
          x.codePointRange.compare(y.codePointRange) match {
            case 0 =>
              x.codePointStatus.compare(y.codePointStatus)
            case otherwise => otherwise
          }
      }

    implicit def orderingInstance: Ordering[Row] = orderInstance.toOrdering

    // Regexes used to extract out a single input code point or a range of
    // input code points.
    private val rangeRegex: Regex =
      """([0-9A-Fa-f]{1,6})\.\.([0-9A-Fa-f]{1,6})""".r
    private val singleRegex: Regex =
      """([0-9A-Fa-f]{1,6})""".r

    /**
     * Give an input code point string (the first column in the UTS-46 lookup table), attempt to
     * parse it into a `CodePointRange`.
     */
    private def parseInputCodePoints(value: String): Either[String, CodePointRange] =
      value.trim match {
        case rangeRegex(lower, upper) =>
          CodePointRange.fromHexStrings(lower, upper)
        case singleRegex(value) =>
          CodePointRange.fromHexString(value)
        case _ =>
          Left(s"Unable to parse as code points: ${value}")
      }

    /**
     * Given a single row/line in the UTS-46 lookup table, attempt to parse it into a `Row`
     * type.
     */
    def fromString(value: String): Either[String, Row] =
      value.split("""[;#]""").toList match {
        case input :: status :: _ :: Nil =>
          for {
            input <- parseInputCodePoints(input)
            status <- CodePointStatus.from(status, MappingValue.empty, None)
          } yield Row(input, status)
        case input :: status :: mapping :: _ :: Nil =>
          for {
            input <- parseInputCodePoints(input)
            mapping <- MappingValue.fromString(mapping)
            status <- CodePointStatus.from(status, mapping, None)
          } yield Row(input, status)
        case input :: status :: mapping :: idna2008Status :: _ :: Nil if mapping.trim.isEmpty =>
          for {
            input <- parseInputCodePoints(input)
            mapping <- MappingValue.fromString(mapping)
            idna2008Status <- IDNA2008Status.fromString(idna2008Status)
            status <- CodePointStatus.from(status, mapping, Some(idna2008Status))
          } yield Row(input, status)
        case _ =>
          Left(s"Unable to parse row: ${value}")
      }
  }

  /**
   * A type representing the parsed UTS-46 table.
   *
   * This is the heart of the code generation. There is a `SortedSet` for each of the methods we
   * will be generating from `CodePointMapperBase`.
   *
   * The `addRow` methods are used to update an instance of this class as we are parsing the
   * Unicode IDNA mapping table file.
   */
  final private case class Rows(
      version: UnicodeVersion,
      deviationIgnored: SortedSet[CodePointRange],
      deviationMapped: SortedSet[(CodePointRange, CodePoint)],
      deviationMultiMapped: SortedSet[(CodePointRange, NonEmptyList[CodePoint])],
      disallowed: SortedSet[CodePointRange],
      disallowedSTD3Mapped: SortedSet[(CodePointRange, CodePoint)],
      disallowedSTD3MultiMapped: SortedSet[(CodePointRange, NonEmptyList[CodePoint])],
      disallowedSTD3Valid: SortedSet[CodePointRange],
      ignored: SortedSet[CodePointRange],
      mapped: SortedSet[(CodePointRange, CodePoint)],
      mappedMulti: SortedSet[(CodePointRange, NonEmptyList[CodePoint])],
      validAlways: SortedSet[CodePointRange],
      validNV8: SortedSet[CodePointRange],
      validXV8: SortedSet[CodePointRange]
  ) {

    /**
     * Add a row. This method will determine the appropriate set the row should be inserted into
     * based on the `CodePointStatus` value.
     */
    def addRow(row: Row): Rows =
      addRow(row.codePointRange, row.codePointStatus)

    /**
     * Add a row. This method will determine the appropriate set the row should be inserted into
     * based on the `CodePointStatus` value.
     */
    def addRow(codePointRange: CodePointRange, codePointStatus: CodePointStatus): Rows = {
      import CodePointStatus._
      codePointStatus match {
        case codePointStatus: Valid =>
          addValid(codePointRange, codePointStatus.idna2008Status)
        case Ignored =>
          addIgnored(codePointRange)
        case codePointStatus: Mapped =>
          addMapped(codePointRange, codePointStatus.mapping)
        case codePointStatus: Deviation =>
          addDeviation(codePointRange, codePointStatus.mapping)
        case Disallowed =>
          addDisallowed(codePointRange)
        case Disallowed_STD3_Valid =>
          addDisallowedSTD3Valid(codePointRange)
        case codePointStatus: Disallowed_STD3_Mapped =>
          addDisallowedSTD3Mapped(codePointRange, codePointStatus.mapping)
      }
    }

    /**
     * Add a valid code point range to the set of `Rows`. The appropriate `SortedSet` to update,
     * always, NV8, or XV8 will be determined.
     */
    def addValid(codePointRange: CodePointRange, idna2008Status: Option[IDNA2008Status]): Rows =
      idna2008Status.fold(
        this.copy(validAlways = validAlways + codePointRange)
      ) {
        case IDNA2008Status.NV8 =>
          this.copy(validNV8 = validNV8 + codePointRange)
        case IDNA2008Status.XV8 =>
          this.copy(validXV8 = validXV8 + codePointRange)
      }

    /**
     * Add a code point range which is ignored in UTS46.
     */
    def addIgnored(codePointRange: CodePointRange): Rows =
      this.copy(ignored = ignored + codePointRange)

    /**
     * Add a code point range which is mapped to one or more alternative code points in UTS 46.
     */
    def addMapped(codePointRange: CodePointRange, mapping: NonEmptyList[CodePoint]): Rows =
      if (mapping.size > 1) {
        this.copy(mappedMulti = mappedMulti + ((codePointRange, mapping)))
      } else {
        this.copy(mapped = mapped + ((codePointRange, mapping.head)))
      }

    /**
     * Add a deviation code point range which may map to zero or more alternative code points in
     * UTS 46.
     */
    def addDeviation(codePointRange: CodePointRange, mapping: List[CodePoint]): Rows =
      mapping match {
        case x :: Nil =>
          this.copy(deviationMapped = deviationMapped + ((codePointRange, x)))
        case x :: xs =>
          this.copy(deviationMultiMapped =
            deviationMultiMapped + ((codePointRange, NonEmptyList(x, xs))))
        case _ =>
          this.copy(deviationIgnored = deviationIgnored + codePointRange)
      }

    /**
     * Add a disallowed code point range.
     */
    def addDisallowed(codePointRange: CodePointRange): Rows =
      this.copy(disallowed = disallowed + codePointRange)

    /**
     * Add a code point range which is disallowed unless useStd3ASCIIRules is false.
     */
    def addDisallowedSTD3Valid(codePointRange: CodePointRange): Rows =
      this.copy(disallowedSTD3Valid = disallowedSTD3Valid + codePointRange)

    /**
     * Add a code point range, which may be mapped if useStd3ASCIIRules is false.
     */
    def addDisallowedSTD3Mapped(
        codePointRange: CodePointRange,
        mapping: NonEmptyList[CodePoint]): Rows =
      mapping.toList match {
        case x :: Nil =>
          this.copy(disallowedSTD3Mapped = disallowedSTD3Mapped + ((codePointRange, x)))
        case _ =>
          this.copy(disallowedSTD3MultiMapped =
            disallowedSTD3MultiMapped + ((codePointRange, mapping)))
      }

    /**
     * Create the AST for the generated file.
     */
    private def asSourceTree: Tree = {
      // The Type of a BitSet
      // val bitSetType: Type =
      //  t"BitSet"

      // Create a Type for an IntMap
      // def intMapType(valueType: Type): Type =
      //  t"IntMap[$valueType]"

      // Create a Type for a NonEmptyList
      def nelType(valueType: Type): Type =
        t"NonEmptyList[$valueType]"

      // A Type for an IntMap[Int]
      // val intMapOfIntType: Type =
      //  intMapType(t"Int")

      // A Type for a NonEmptyList[Int]
      val nelOfIntType: Type =
        nelType(t"Int")

      // A Type for an IntMap[NonEmptyList[Int]]
      // val intMapOfNELOfIntType: Type =
      //  intMapType(nelOfIntType)

      // Convert a NonEmptyList[Int] into a AST which would generate the same
      // NonEmptyList[Int]
      def nelToTree(value: NonEmptyList[Int]): Term =
        q"NonEmptyList.of(..${value.toList.map(Lit.Int.apply)})"

      // Convert a CodePointRange into an inclusive Range.
      def rangeInclusiveTree(codePointRange: CodePointRange): Term =
        q"Range.inclusive(${Lit.Int(codePointRange.lower.value)}, ${Lit.Int(codePointRange.upper.value)})"

      // Convert a Foldable of CodePointRange into an expresssion which will
      // result in a BitSet containing all the code points.
      //
      // Note: Care must be taken in how this is constructed. There are
      // several valid encodings which create too deeply nested ASTs which
      // cause the ScalaMeta printer to crash.
      def asBitSetTerm(fa: SortedSet[CodePointRange]): Term = {
        val (ranges, singles): (List[Term], List[Term]) = fa.foldMap(value =>
          if (value.size < 10) {
            // Unwrap relatively small ranges so we can create them all at
            // once with a big BitSet literal. We don't do this for everything
            // because we start running into method size limitations.
            (
              Nil,
              Range.inclusive(value.lower.value, value.upper.value).foldLeft(List.empty[Term]) {
                case (acc, value) => q"${Lit.Int(value)}" +: acc
              })
          } else {
            (List(q"${rangeInclusiveTree(value)}"), Nil)
          })

        if (ranges.isEmpty && singles.isEmpty) {
          q"BitSet.empty"
        } else if (ranges.isEmpty) {
          q"BitSet(..$singles).compact"
        } else {
          q"List(..$ranges).foldLeft(BitSet(..$singles)){case (acc, range) => acc | BitSet.fromScalaRange(range)}.compact"
        }
      }

      // Convert a mapping of code point ranges to terms, usually Int, or
      // NonEmptyList[Int], into an expression which will yield an IntMap of
      // these terms.
      //
      // Note: Care must be taken in how this is constructed. There are
      // several valid encodings which create too deeply nested ASTs which
      // cause the ScalaMeta printer to crash.
      def asIntMapTerm(fa: SortedMap[CodePointRange, Term], valueType: Type): Term = {
        val (ranges, singles): (List[Term], List[Term]) =
          fa.foldLeft((List.empty[Term], List.empty[Term])) {
            case ((r, s), (codePointRange, result)) =>
              if (codePointRange.size === 1) {
                (r, q"(${Lit.Int(codePointRange.lower.value)}, ${result})" +: s)
              } else {
                (q"(${rangeInclusiveTree(codePointRange)}, $result)" +: r, s)
              }
          }

        q"""List[(Range, $valueType)](..$ranges).foldLeft(
         IntMap[${valueType}](..$singles)
        ){
          case (acc, (range, result)) =>
            range.foldLeft(acc){
              case (acc, value) =>
                acc.updated(value, result)
            }
         }"""
      }

      // Create a val definition for one of the methods
      def emitMethod(name: String, rhs: Term): Defn.Val =
        q"protected override final lazy val ${Pat.Var(Term.Name(name))} = $rhs"

      // For a set of code points which map to a single result code point,
      // create the expression that yields an IntMap[Int] of that mapping.
      def singleMappingToTrees(value: SortedSet[(CodePointRange, CodePoint)]): Term =
        asIntMapTerm(
          value.foldLeft(SortedMap.empty[CodePointRange, Term]) {
            case (acc, (codePointRange, mapping)) =>
              acc + (codePointRange -> Lit.Int(mapping.value))
          },
          t"Int"
        )

      // For a set of code points which map to multiple code points, create
      // the expression that yields an IntMap[NonEmptyList[Int]].
      def multiMappingToTrees(
          value: SortedSet[(CodePointRange, NonEmptyList[CodePoint])]): Term =
        asIntMapTerm(
          value.foldLeft(SortedMap.empty[CodePointRange, Term]) {
            case (acc, (codePointRange, mapping)) =>
              acc + (codePointRange -> nelToTree(mapping.map(_.value)))
          },
          nelOfIntType
        )

      // Create the expressions for the various methods we are overriding.

      def validAlwaysMethod: Defn.Val =
        emitMethod("validAlways", asBitSetTerm(validAlways))

      def validNV8Method: Defn.Val =
        emitMethod("validNV8", asBitSetTerm(validNV8))

      def validXV8Method: Defn.Val =
        emitMethod("validXV8", asBitSetTerm(validXV8))

      def ignoredMethod: Defn.Val =
        emitMethod("ignored", asBitSetTerm(ignored))

      def disallowedMethod: Defn.Val =
        emitMethod("disallowed", asBitSetTerm(disallowed))

      def deviationIgnoredMethod: Defn.Val =
        emitMethod("deviationIgnored", asBitSetTerm(deviationIgnored))

      def disallowedSTD3ValidMethod: Defn.Val =
        emitMethod("disallowedSTD3Valid", asBitSetTerm(disallowedSTD3Valid))

      def mappedMultiMethod: Defn.Val =
        emitMethod("mappedMultiCodePoints", multiMappingToTrees(mappedMulti))

      def deviationMappedMethod: Defn.Val =
        emitMethod("deviationMapped", singleMappingToTrees(deviationMapped))

      def deviationMultiMappedMethod: Defn.Val =
        emitMethod("deviationMultiMapped", multiMappingToTrees(deviationMultiMapped))

      def disallowedSTD3MappedMethod: Defn.Val =
        emitMethod("disallowedSTD3Mapped", singleMappingToTrees(disallowedSTD3Mapped))

      def disallowedSTD3MultiMappedMethod: Defn.Val =
        emitMethod("disallowedSTD3MultiMapped", multiMappingToTrees(disallowedSTD3MultiMapped))

      // Note: Things are different for the `def mapped: IntMap[Int]`
      // method. This is by far the largest method we will be generating. It
      // requires breaking up the input code points into four private methods
      // which are combined to yield the final result method.
      def mappedMethods: List[Defn] = {
        val (mapped0, mapped1, mapped2, mapped3) = mapped.splitAt(mapped.size / 2) match {
          case (a, b) =>
            (a.splitAt(a.size / 2), b.splitAt(b.size / 2)) match {
              case ((a, b), (c, d)) =>
                (a, b, c, d)
            }
        }

        List(
          q"private final def mapped0 = ${singleMappingToTrees(mapped0)}",
          q"private final def mapped1 = ${singleMappingToTrees(mapped1)}",
          q"private final def mapped2 = ${singleMappingToTrees(mapped2)}",
          q"private final def mapped3 = ${singleMappingToTrees(mapped3)}",
          q"protected override final lazy val mapped = mapped0 ++ mapped1 ++ mapped2 ++ mapped3"
        )
      }

      // Emit the source.
      //
      // Note: The methods `mapped` and `mappedMulti` are generated in a
      // precursor class `GeneratedCodePointMapper0` which
      // `GeneratedCodePointMapper` extends. This is because the having all
      // the methods in the same class caused the `<init>` method of the class
      // to exceed the valid method size for a JVM class.

      source"""package org.typelevel.idna4s.core.uts46

import scala.collection.immutable.IntMap
import cats.data.NonEmptyList
import cats.collections.BitSet

private[uts46] abstract class GeneratedCodePointMapper0 extends CodePointMapperBase {
  override final val unicodeVersion = ${Lit.String(version.asString)}
  $mappedMultiMethod
  ..$mappedMethods
}

private[uts46] abstract class GeneratedCodePointMapper extends GeneratedCodePointMapper0 {
           $validAlwaysMethod
           $validNV8Method
           $validXV8Method
           $ignoredMethod
           $disallowedMethod
           $deviationIgnoredMethod
           $disallowedSTD3ValidMethod
           $deviationMappedMethod
           $deviationMultiMappedMethod
           $disallowedSTD3MappedMethod
           $disallowedSTD3MultiMappedMethod
       }"""
    }

    /**
     * Given a package name, generate the `String` content of the generated source file.
     */
    def asSourceFile: String = asSourceTree.syntax
  }

  private object Rows {

    // Regex for parsing the unicode version string from the file.
    val unicodeVersionRegex: Regex =
      """#\s*Version:\s*(\d+\.\d+\.\d+)""".r

    /**
     * Create an empty instance of `Rows`, given some Unicode version.
     */
    def empty(version: UnicodeVersion): Rows =
      Rows(
        version,
        SortedSet.empty,
        SortedSet.empty,
        SortedSet.empty,
        SortedSet.empty,
        SortedSet.empty,
        SortedSet.empty,
        SortedSet.empty,
        SortedSet.empty,
        SortedSet.empty,
        SortedSet.empty,
        SortedSet.empty,
        SortedSet.empty,
        SortedSet.empty
      )

    /**
     * Parse the rows from a list of lines.
     */
    def fromLines(lines: List[String]): Either[String, Rows] = {

      @tailrec
      def parseVersion(lines: List[String]): Either[String, (UnicodeVersion, List[String])] =
        lines match {
          case Nil =>
            Left("End of input reached without finding version string.")
          case x :: xs =>
            x match {
              case unicodeVersionRegex(version) =>
                UnicodeVersion.fromString(version).map((_, xs))
              case _ => parseVersion(xs)
            }
        }

      type F[A] = Either[String, A]

      parseVersion(lines) match {
        case Left(error) => Left(error)
        case Right((version, rest)) =>
          // Drop comments after parsing the version
          rest.dropWhile(_.startsWith("#")).foldM[F, Rows](Rows.empty(version)) {
            case (acc, value) if value.trim.isEmpty || value.trim.startsWith("#") =>
              Right(acc)
            case (acc, value) =>
              Row.fromString(value).map(acc.addRow).leftMap(error => s"Error at $value: $error")
          }
      }
    }

    /**
     * Download the UTS-46 lookup table from the given URL and parse it into rows.
     */
    def fromURL(url: String): Either[Throwable, Rows] =
      Either
        .catchNonFatal(URI.create(url).toURL())
        .flatMap(url => Either.catchNonFatal(IO.readLinesURL(url)))
        .flatMap(lines =>
          fromLines(lines).fold(
            e => Left(new RuntimeException(e)),
            rows => Right(rows)
          ))

    /**
     * Generate the mapping table code by downloading the mappings from `www.unicode.org`.
     *
     * @param version
     *   The version of Unicode to use to generate the mapping table code, if `None`, then
     *   "latest" will be used. This is the recommended usage as Unicode will post pre-release
     *   versions on their site that we probably don't want to implement a release against.
     */
    def fromUnicodeURL(version: Option[UnicodeVersion]): Either[Throwable, Rows] = {
      def makeUrl(rawVersion: String): String =
        s"https://www.unicode.org/Public/idna/${rawVersion}/IdnaMappingTable.txt"

      val url: String =
        version.fold(
          makeUrl("latest")
        )(version => makeUrl(version.asString))

      fromURL(url).flatMap(rows =>
        // Validate that if an explicit version was specified, that is what we
        // parsed.
        version.fold(
          Right(rows): Either[Throwable, Rows]
        )(version =>
          if (rows.version === version) {
            Right(rows)
          } else {
            left(new RuntimeException(
              s"Expected to get the mapping table for version ${version}, but got ${rows.version}."))
          }))
    }

    /**
     * Generate the mapping table code by downloading the mappings from `www.unicode.org`. The
     * "latest" version of Unicode will be used, and parsed from the file.
     */
    def fromUnicodeURL: Either[Throwable, Rows] =
      fromUnicodeURL(None)

    /**
     * Generate the mapping table code by downloading the mappings from `www.unicode.org`.
     *
     * @param version
     *   The version of Unicode to use to generate the mapping table code.
     */
    def fromUnicodeURL(version: UnicodeVersion): Either[Throwable, Rows] =
      fromUnicodeURL(Some(version))
  }
}
