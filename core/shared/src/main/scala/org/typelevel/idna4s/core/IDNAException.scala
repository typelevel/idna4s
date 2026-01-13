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

package org.typelevel.idna4s.core

import cats._

/**
 * Marker trait for IDNA errors.
 *
 * Errors in idna4s are usually scoped to sub-domains, e.g. a class of errors for Bootstring, a
 * class of errors for UTS-46 code point mapping, etc. In many cases we are mandated by the
 * relevant specifications to return an error ''and'' continue processing, so we often end up in
 * a situation where we have a partial result and more than one error.
 *
 * We use the [[IDNAException]] marker trait so that we can express certain errors in their
 * domain specific representation, but still aggregate different domains together, e.g. `((a:
 * NonEmptyList[BootStringException]).widen ++ (b: NonEmptyList[MappingException]).widen):
 * NonEmptyList[IDNAException]`.
 */
trait IDNAException extends RuntimeException

object IDNAException {
  implicit val showForIDNAException: Show[IDNAException] =
    Show.fromToString

  /**
   * Exception used both in Bootstring and UTS-46 and which I doubt will ever be thrown.
   */
  final private[idna4s] class UnableToResizeBufferException extends IDNAException {
    override val getMessage: String =
      s"Can not resize buffer as it would exceed largest valid size ${Int.MaxValue}. What are you doing?"

    final override def toString: String =
      s"UnableToResizeBufferException(${getMessage})"
  }
}
