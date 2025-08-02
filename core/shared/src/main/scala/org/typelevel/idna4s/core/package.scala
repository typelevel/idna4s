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

import cats.data._
import java.nio.Buffer
import java.nio.CharBuffer
import java.nio.IntBuffer
import scala.annotation.tailrec

package object core {
  // General utilities for working with code points

  private[idna4s] def foldLeftCodePoints[A](value: String)(base: A)(f: (A, Int) => A): A = {
    val len: Int = value.length

    @tailrec
    def loop(index: Int, acc: A): A =
      if (index >= len) {
        acc
      } else {
        val codePoint: Int = value.codePointAt(index)
        val indexIncrement: Int = if (codePoint >= 0x10000) 2 else 1

        loop(index + indexIncrement, f(acc, codePoint))
      }

    loop(0, base)
  }

  private[idna4s] def codePointsAsChain(value: String): Chain[Int] =
    foldLeftCodePoints(value)(Chain.empty[Int]) {
      case (acc, value) =>
        acc :+ value
    }

  private[idna4s] def codePointsAsBuffer(value: String): IntBuffer = {
    val result: IntBuffer = foldLeftCodePoints(value)(IntBuffer.allocate(value.length)) {
      case (acc, value) =>
        acc.put(value)
    }

    result.flip
    result
  }

  private[idna4s] def stringFromCodePoints(value: IntBuffer): Either[String, String] = {
    val out: CharBuffer = CharBuffer.allocate(value.remaining * 2)

    @tailrec
    def loop: Either[String, String] =
      if (value.hasRemaining) {
        value.get match {
          case value if value > 0x10ffff =>
            Left(s"Encountered int value which is > max valid Unicode codepoint: ${value}")
          case value =>
            out.put(Character.toChars(value))
            loop
        }
      } else {
        Right(out.flip.toString)
      }

    loop
  }

  /**
   * This method sets the position on a `Buffer`. This gets around a widening in the Java 8 API.
   */
  private[idna4s] def position[A <: Buffer](buffer: A, position: Int): A = {
    buffer.position(position) // This is a `Buffer` not `A` on JRE 8.
    buffer
  }

  /**
   * This is an implementation of the Absolute bulk put operation added in JRE 16 using methods
   * available on JRE 8.
   */
  private[idna4s] def put(
      buffer: IntBuffer)(index: Int, src: IntBuffer, offset: Int, length: Int): IntBuffer = {
    val oldPosition: Int = buffer.position()

    buffer.position(index)

    buffer.put(src.array, offset, length)

    buffer.position(oldPosition)

    buffer
  }

  // scalafix:off
  final private[this] val HalfIntMaxValue = Int.MaxValue / 2
  // scalafix:on

  /**
   * Calculate the a new size for an `IntBuffer` so that it can accept at ''least'' the given
   * new capacity.
   *
   * If the buffer is already at or exceeding the required size, then the buffer's current size
   * is returned. Otherwise attempt to double the buffer's size as long as that won't overflow.
   * If we can not double it, add `neededSize - remaining` to the current capacity. In the
   * unbelievable case where `buffer.remaining + neededSize > Int.MaxValue`, then yield an
   * error.
   */
  @inline
  private def calculateNewSize(buffer: IntBuffer, neededSize: Int): Int =
    if (buffer.remaining >= neededSize) {
      // This will be the branch most often hit by a wide margin.
      buffer.capacity
    } else if (buffer.capacity <= HalfIntMaxValue && buffer.capacity + buffer.remaining >= neededSize) {
      // Double it
      buffer.capacity * 2
    } else if (neededSize.toLong - buffer.remaining.toLong <= Int.MaxValue.toLong) {
      // I do not expect this branch will ever be executed under normal
      // circumstances.
      neededSize - buffer.remaining
    } else {
      // I do not expect this branch will ever be executed under normal
      // circumstances.
      throw new IDNAException.UnableToResizeBufferException
    }

  /**
   * Copy the contents of a given `IntBuffer` into a new `IntBuffer` with double capacity if the
   * given `IntBuffer` is at capacity, unless doubling it would overflow, in that case attempt
   * to just add the minimum needed allocation, if that is not possible then throw an error.
   *
   * The error case should only happen if there is a bug or someone is intentionally abusing the
   * system. We need to handle it as it could be used to influence the result to potentially
   * change a URI.
   */
  @inline
  private[idna4s] def maybeResize(buffer: IntBuffer, neededSize: Int): IntBuffer =
    if (buffer.remaining >= neededSize) {
      // This will be the branch most often hit by a wide margin.
      buffer
    } else {
      val pos: Int = buffer.position
      val newSize: Int = calculateNewSize(buffer, neededSize)

      // Shadow here is because `(buffer: IntBuffer).position(pos): Buffer`
      // but we want `IntBuffer`, e.g. it is getting widened to the super
      // type.
      IntBuffer.allocate(newSize).put(buffer.array) match {
        case buffer =>
          buffer.position(pos)
          buffer
      }
    }
}
