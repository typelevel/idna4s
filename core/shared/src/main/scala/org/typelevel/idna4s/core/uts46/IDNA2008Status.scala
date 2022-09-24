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

import cats._

/**
 * For code points which are valid under UTS-46 mapping, this type indicates if they excluded
 * under IDNA 2008.
 */
sealed abstract class IDNA2008Status extends Serializable

object IDNA2008Status {

  /**
   * Excluded from the current version of Unicode.
   */
  case object NV8 extends IDNA2008Status

  /**
   * Excluded from all versions of Unicode.
   */
  case object XV8 extends IDNA2008Status

  implicit val orderAndHashForIDNA2008Status: Order[IDNA2008Status] with Hash[IDNA2008Status] =
    new Order[IDNA2008Status] with Hash[IDNA2008Status] {
      override def hash(x: IDNA2008Status): Int =
        x.hashCode

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

  implicit def orderingForIDNA2008Status: Ordering[IDNA2008Status] =
    orderAndHashForIDNA2008Status.toOrdering

  implicit val showForIDNA2008Status: Show[IDNA2008Status] =
    Show.fromToString
}
