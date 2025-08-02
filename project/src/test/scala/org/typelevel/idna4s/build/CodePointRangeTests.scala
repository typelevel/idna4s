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

import cats.syntax.all._
import munit._
import org.scalacheck.Prop._
import org.scalacheck._
import org.typelevel.idna4s.build.ScalacheckInstances._
import scala.collection.immutable.SortedMap

final class CodePointRangeTests extends ScalaCheckSuite {
  property("overlapsWith is consistent with difference") {
    forAll((a: CodePointRange, b: CodePointRange) =>
      if (a.overlapsWith(b)) {
        // CodePointRange doesn't permit empty ranges, so at least one
        // difference will always be nonEmpty if they overlap.
        (
          (Prop(a.difference(b).nonEmpty)) ||
            (Prop(b.difference(a).nonEmpty))
        ) :| "a.difference(b).nonEmpty || b.difference(a).nonEmpty"
      } else {
        Prop.passed
      })
  }

  property("resolveMissingMapping yields no overlapping ranges") {
    forAll { (value: List[(CodePointRange, Int)]) =>
      val noOverlapping: SortedMap[CodePointRange, Int] =
        CodePointRange.resolveMissingMapping(value)

      noOverlapping.foldLeft(Prop.passed) {
        case (acc, a @ (k1, _)) =>
          noOverlapping.foldLeft(acc) {
            case (acc, b) if a === b =>
              acc
            case (_, b @ (k2, _)) =>
              (
                (Prop(k1.overlapsWith(k2) === false) :| s"$k1.overlapsWith($k2) == false") &&
                  (Prop(k2.overlapsWith(k1) === false) :| s"$k2.overlapsWith($k1) == false")
              ) :| s"$a does not overlap with $b"
          } :| s"$a overlap correctness"
      } :| s"Non-overlapping map: ${noOverlapping}"
    }
  }

  property("overlap commutative") {
    forAll((x: CodePointRange, y: CodePointRange) => x.overlapsWith(y) ?= y.overlapsWith(x))
  }

  property("left difference correctness") {
    forAll((x: CodePointRange, y: CodePointRange) =>
      (if (x.leftDifference(y).nonEmpty) {
         Prop(x.lower < y.lower) :| s"$x.lower < $y.lower"
       } else {
         Prop(x.lower >= y.lower) :| s"$x.lower >= $y.lower"
       }) :| s"Left difference implies x.lower < y.lower for x=$x, y=$y")
  }

  property("right difference correctness") {
    forAll((x: CodePointRange, y: CodePointRange) =>
      (if (x.rightDifference(y).nonEmpty) {
         Prop(x.upper > y.upper) :| s"$x.upper > $y.upper"
       } else {
         Prop(x.upper <= y.upper) :| s"$x.upper <= $y.upper"
       }) :| s"Right difference implies x.upper > y.upper for x=$x, y=$y")
  }
}
