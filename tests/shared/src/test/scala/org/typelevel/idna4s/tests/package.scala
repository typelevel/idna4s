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

package org.typelevel.idna4s

import cats._
import cats.syntax.all._
import org.typelevel.idna4s.core._
import scala.annotation.tailrec

package object tests {

  /**
   * Perform some reduction operation over all Unicode code points.
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

  /**
   * Perform some reduction operation over all Unicode code points, where the results have a
   * `Semigroup` and can be merged.
   */
  private[tests] def reduceMapCodePoints[A: Semigroup](f: CodePoint => A): A =
    reduceCodePoints[A](f, (cp, acc) => acc |+| f(cp))
}
