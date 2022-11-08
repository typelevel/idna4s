package org.typelevel.idna4s.build

import cats.syntax.all._
import munit._
import org.scalacheck.Prop._
import org.scalacheck._
import org.typelevel.idna4s.build.ScalacheckInstances._
import scala.collection.immutable.SortedMap

final class CodePointRangeTests extends ScalaCheckSuite {
  property("overlap and difference correctness"){
    forAll{(value: List[(CodePointRange, Int)]) =>
      val noOverlapping: SortedMap[CodePointRange, Int] = CodePointRange.resolveMissingMapping(value)

      noOverlapping.foldLeft(Prop.passed){
        case (acc, a@(k1, _)) =>
          noOverlapping.foldLeft(acc){
            case (acc, b) if a === b =>
              acc
            case (acc, b@(k2, _)) =>
              (
                (Prop(k1.overlapsWith(k2) === false) :| s"$k1.overlapsWith($k2) == false") &&
                (Prop(k2.overlapsWith(k1) === false) :| s"$k2.overlapsWith($k1) == false")
              ) :| s"$a does not overlap with $b"
          } :| s"$a overlap correctness"
      } :| s"Non-overlapping map: ${noOverlapping}"
    }
  }

  property("overlap commutative"){
    forAll((x: CodePointRange, y: CodePointRange) =>
      x.overlapsWith(y) ?= y.overlapsWith(x)
    )
  }

  property("left difference correctness"){
    forAll((x: CodePointRange, y: CodePointRange) =>
      (if (x.leftDifference(y).nonEmpty) {
        Prop(x.lower < y.lower) :| s"$x.lower < $y.lower"
      } else {
        Prop(x.lower >= y.lower) :| s"$x.lower >= $y.lower"
      }) :| s"Left difference implies x.lower < y.lower for x=$x, y=$y"
    )
  }

  property("right difference correctness"){
    forAll((x: CodePointRange, y: CodePointRange) =>
      (if (x.rightDifference(y).nonEmpty) {
        Prop(x.upper > y.upper) :| s"$x.upper > $y.upper"
      } else {
        Prop(x.upper <= y.upper) :| s"$x.upper <= $y.upper"
      }) :| s"Right difference implies x.upper > y.upper for x=$x, y=$y"
    )
  }
}
