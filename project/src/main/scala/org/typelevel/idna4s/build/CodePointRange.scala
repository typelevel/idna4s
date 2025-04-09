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
import scala.collection.immutable.SortedMap

/**
 * A type representing an inclusive Range of code points. This is similar to `Range`, but is
 * used instead because `Range` has unusual equality and ordering properties and by assuming we
 * have a non-empty, inclusive, range it was much easier to write an `Order` instance.
 */
sealed abstract private[build] class CodePointRange extends Serializable {
  def lower: CodePoint
  def upper: CodePoint

  /**
   * Check to see if this code point range entirelly to the left of (closer to 0) the other
   * range.
   */
  final def isLeftOf(that: CodePointRange): Boolean =
    upper < that.lower

  /**
   * Check to see if this code point range entirelly to the right of (further from 0) the other
   * range.
   */
  final def isRightOf(that: CodePointRange): Boolean =
    that.upper < lower

  /**
   * Check to see if this code point range overlaps with the other code point range.
   */
  final def overlapsWith(that: CodePointRange): Boolean =
    (isLeftOf(that) || isRightOf(that)) === false

  /**
   * Create the left difference between this range and the given range.
   *
   * This is a subset of the set difference, when considering code point ranges as sets of code
   * points. The left/right notion here is used because the result is represented as a range the
   * operation B - A may yield 0, 1, or 2 ranges. 0 for the empty set, 1 for a partial overlap
   * where B extends to the left of A xor the right of A, and 2 where A is a strict non-empty
   * subset of B.
   */
  final def leftDifference(that: CodePointRange): Option[CodePointRange] =
    if (lower < that.lower) {
      Some(CodePointRange.unsafeFromInts(lower.value, upper.value.min(that.lower.value - 1)))
    } else {
      None
    }

  /**
   * Create the right difference between this range and the given range.
   *
   * This is a subset of the set difference, when considering code point ranges as sets of code
   * points. The left/right notion here is used because the result is represented as a range the
   * operation B - A may yield 0, 1, or 2 ranges. 0 for the empty set, 1 for a partial overlap
   * where B extends to the left of A xor the right of A, and 2 where A is a strict non-empty
   * subset of B.
   */
  final def rightDifference(that: CodePointRange): Option[CodePointRange] =
    if (that.upper < upper) {
      Some(CodePointRange.unsafeFromInts((that.upper.value + 1).max(lower.value), upper.value))
    } else {
      None
    }

  /**
   * The set difference between this range an the given range.
   *
   * Because we are dealing with ranges, this yields two distinct values for the code points
   * which are left of given range and the code points which are right of the given range,
   * either or both of which may be empty.
   */
  final def difference(that: CodePointRange): Option[Ior[CodePointRange, CodePointRange]] =
    (leftDifference(that), rightDifference(that)) match {
      case (Some(l), Some(r)) =>
        Option(Ior.both(l, r))
      case (Some(l), _) =>
        Option(Ior.left(l))
      case (_, Some(r)) =>
        Option(Ior.right(r))
      case _ =>
        None
    }

  final def size: Int = upper.value - lower.value + 1

  final override def toString: String =
    if (size === 1) {
      s"CodePointRange($lower)"
    } else {
      s"CodePointRange($lower, $upper)"
    }
}

private[build] object CodePointRange {

  final private[this] case class CodePointRangeImpl(
      override val lower: CodePoint,
      override val upper: CodePoint)
      extends CodePointRange

  final case class Single(value: CodePoint) extends CodePointRange {
    override def lower: CodePoint = value
    override def upper: CodePoint = value
  }

  object Single {
    implicit def hashAndOrderForSingle: Hash[Single] with Order[Single] =
      new Hash[Single] with Order[Single] {
        override def hash(x: Single): Int =
          hashAndOrderForCodePointRange.hash(x)

        override def compare(x: Single, y: Single): Int =
          hashAndOrderForCodePointRange.compare(x, y)
      }

    implicit def orderingInstance: Ordering[Single] =
      hashAndOrderForSingle.toOrdering

    implicit def showInstance: Show[Single] =
      Show[CodePointRange].narrow[Single]
  }

  /**
   * Given some `Foldable` of [[CodePointRange]] values mapped to other values of A, create a
   * `SortedMap` where overlapping values defined later in the `Foldable` replace the overlapped
   * sections defined earlier.
   *
   * For example,
   * {{{
   * def a = CodePointRange.unsafeFromInts(0, 10)
   * def b = CodePointRange.unsafeFromInts(5, 10)
   * def c = List((a -> "haskell"), (b -> "curry"))
   * def d = CodePointRange.resolveMissingMapping(c)
   *
   * d.foreach(println)
   *
   * // yields
   * // (CodePointRange(CodePoint(value = 0), CodePoint(value = 4)),haskell)
   * // (CodePointRange(CodePoint(value = 5), CodePoint(value = 10)),curry)
   * //
   * // Where the mappings for code points 5 to 10 to "haskell" have been replaced by mappings to "curry".
   * }}}
   *
   * This method is used for handling the Unicode 15.0 behavior of the @missing mapping values.
   *
   * @see
   *   [[https://www.unicode.org/reports/tr44/#Missing_Conventions]]
   */
  final def resolveMissingMapping[F[_]: Foldable, A](
      fa: F[(CodePointRange, A)]): SortedMap[CodePointRange, A] =
    fa.foldLeft(SortedMap.empty[CodePointRange, A]) {
      case (acc, kv @ (range, _)) =>
        // Check each current mapping to see if it overlaps with the new,
        // higher priority, mapping.  This makes this O(n^2). A much more
        // efficient solution is possible if we had an interval tree data
        // type. Discussions are in progress about adding one to
        // cats-collections, but in the mean time we will use this.
        val value = acc.foldLeft(SortedMap.empty[CodePointRange, A]) {
          case (acc, (k, v)) if range.overlapsWith(k) =>
            k.difference(range)
              .fold(
                // If the difference is empty, then the more specific value
                // completely overrides the more general value and then we
                // just replace the current mapping. This is done by merely
                // not remapping the current mapping.
                acc
              )(
                _.fold(
                  l => acc + (l -> v),
                  r => acc + (r -> v),
                  (l, r) => (acc + (l -> v)) + (r -> v)
                )
              )
          case (acc, kv) =>
            acc + kv
        }

        value + kv
    }

  def apply(value: CodePoint): CodePointRange =
    Single(value)

  def from(lower: CodePoint, upper: CodePoint): Either[String, CodePointRange] =
    if (lower === upper) {
      Right(apply(lower))
    } else if (lower <= upper) {
      Right(CodePointRangeImpl(lower, upper))
    } else {
      Left(s"Invalid CodePointRange, lower must be <= upper: [$lower, $upper]")
    }

  def unsafeFrom(lower: CodePoint, upper: CodePoint): CodePointRange =
    from(lower, upper).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )

  def fromInt(value: Int): Either[String, CodePointRange] =
    CodePoint.fromInt(value).map(apply)

  def fromInts(lower: Int, upper: Int): Either[String, CodePointRange] =
    for {
      lower <- CodePoint.fromInt(lower)
      upper <- CodePoint.fromInt(upper)
      result <- from(lower, upper)
    } yield result

  def unsafeFromInts(lower: Int, upper: Int): CodePointRange =
    fromInts(lower, upper).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )

  def fromHexString(value: String): Either[String, CodePointRange] =
    CodePoint.fromHexString(value).map(apply)

  def fromHexStrings(lower: String, upper: String): Either[String, CodePointRange] =
    for {
      lower <- CodePoint.fromHexString(lower)
      upper <- CodePoint.fromHexString(upper)
      result <- from(lower, upper)
    } yield result

  implicit val hashAndOrderForCodePointRange: Order[CodePointRange] with Hash[CodePointRange] =
    new Order[CodePointRange] with Hash[CodePointRange] {
      override def hash(x: CodePointRange): Int = x.hashCode

      // Note, this order is correct in terms of the Order laws, but not
      // correct in terms of the formal definition of mathematical interval
      // ordering. Intervals may only be given a partial ordering. When two
      // intervals overlap, their ordering is undefined. This is okay, for
      // now, because we want to use CodePointRange in SortedMap and
      // SortedSet, so care must be taken to not insert overlapping intervals
      // in most cases. What we really want is a map like structure based in
      // interval trees, but sadly no such structure currently exists in
      // Scala. See https://en.wikipedia.org/wiki/Interval_tree.
      //
      // Some future work might be to define such a structure in
      // cats-collections.
      override def compare(x: CodePointRange, y: CodePointRange): Int =
        x.lower.compare(y.lower) match {
          case 0 =>
            x.upper.compare(y.upper)
          case otherwise =>
            otherwise
        }
    }

  implicit def orderingInstance: Ordering[CodePointRange] =
    hashAndOrderForCodePointRange.toOrdering

  implicit val showForCodePointRange: Show[CodePointRange] =
    Show.fromToString
}
