package org.typelevel.idna4s

import cats._
import cats.syntax.all._
import org.typelevel.idna4s.core._
import scala.annotation.tailrec

package object tests {

  /** Perform some reduction operation over all Unicode code points.
    */
  private[tests] def reduceCodePoints[A](f: CodePoint => A, g: (CodePoint, A) => A): A = {

    @tailrec
    def loop(i: CodePoint, acc: Option[A]): A = {
      def a: A =
        acc.fold(
          f(i)
        )(g(i, _))

      if (i === CodePoint.MaxValue) {
        a
      } else {
        loop(CodePoint.unsafeFromInt(i.value + 1), Some(a))
      }
    }

    loop(CodePoint.MinValue, None)
  }

  /** Perform some reduction operation over all Unicode code points, where the
    * results have a `Semigroup` and can be merged.
    */
  private[tests] def reduceMapCodePoints[A: Semigroup](f: CodePoint => A): A =
    reduceCodePoints[A](f, (cp, acc) => acc |+| f(cp))
}
