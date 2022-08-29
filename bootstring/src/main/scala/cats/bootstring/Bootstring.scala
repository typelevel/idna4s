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

package org.typelevel.idna4s.bootstring

import scala.annotation.tailrec
import cats.syntax.all._
import java.nio.IntBuffer
import scala.collection.immutable.SortedSet
import scala.collection.immutable.SortedMap
import java.lang.Math

object Bootstring {

  private val HalfIntMaxValue: Int = Int.MaxValue / 2

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
      throw new RuntimeException(
        s"Can not resize buffer as it would exceed largest valid size ${Int.MaxValue}. What are you doing?")
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
  private def maybeResize(buffer: IntBuffer, neededSize: Int): IntBuffer =
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

  /**
   * Bootstring encode given `String`.
   *
   * @param params
   *   the [[BootstringParams]] to use.
   * @param value
   *   the `String` to encode.
   */
  def encodeRaw(
      params: BootstringParams
  )(
      value: String
  ): Either[String, String] = {

    // Insert a code point into an `IntBuffer`, automatically resizing it, if
    // the `IntBuffer` is at capacity.
    def insertCodePoint(buffer: IntBuffer, codePoint: Int): IntBuffer =
      maybeResize(buffer, 1).put(codePoint)

    @tailrec
    def encodeCodePoint(buffer: IntBuffer, bias: Bias, q: Int, k: Int): IntBuffer = {
      val threshold: Int =
        if (k <= bias.value + params.tmin.value) {
          params.tmin.value
        } else if (k >= (bias.value + params.tmax.value)) {
          params.tmax.value
        } else {
          k - bias.value
        }

      if (q < threshold) {
        insertCodePoint(buffer, params.base.unsafeIntToCodePointDigit(q))
      } else {
        val qt: Int = q - threshold
        val bt: Int = params.base.value - threshold
        encodeCodePoint(
          insertCodePoint(buffer, params.base.unsafeIntToCodePointDigit(threshold + (qt % bt))),
          bias,
          q = qt / bt,
          k = k + params.base.value
        )
      }
    }

    try {
      val (basicCodePoints, nonBasicCodePoints) = foldLeftCodePoints(value)(
        (IntBuffer.allocate(value.length * 2), SortedSet.empty[Int])) {
        case ((buffer, nonBasic), codePoint) =>
          if (params.isBasicCodePoint(codePoint)) {
            (buffer.put(codePoint), nonBasic)
          } else if (codePoint < params.initialN) {
            // Only occurs in unusual Bootstring usage.
            throw new RuntimeException(
              s"Input contains a non-basic code point < the initial N value. Code Point: ${codePoint}, Initial N: ${params.initialN}.")
          } else {
            (buffer, nonBasic + codePoint)
          }
      }
      val basicCodePointCount: Int = basicCodePoints.position()

      // Insert the delimiter if there is at least one basic code point
      if (basicCodePointCount =!= 0) {
        basicCodePoints.put(params.delimiter.codePoint)
      }

      nonBasicCodePoints.foldLeft(
        (basicCodePoints, params.initialN, 0, basicCodePointCount, params.initialBias)) {
        case ((buffer, previousCodePoint, delta, h, bias), codePoint) =>
          Math.addExact(
            delta,
            Math.multiplyExact(codePoint - previousCodePoint, Math.addExact(h, 1))) match {
            case delta =>
              foldLeftCodePoints(value)((buffer, bias, delta, h)) {
                case ((buffer, bias, delta, h), cp) =>
                  if (cp < codePoint) {
                    (buffer, bias, delta + 1, h)
                  } else if (cp === codePoint) {
                    (
                      encodeCodePoint(buffer, bias, delta, params.base.value),
                      params.unsafeAdaptBias(delta, h + 1, h === basicCodePointCount),
                      0,
                      h + 1)
                  } else {
                    (buffer, bias, delta, h)
                  }
              } match {
                case (buffer, bias, delta, h) =>
                  if (delta === Int.MaxValue) {
                    throw new RuntimeException(
                      "Delta will overflow if encoding continues, this probably means you are attempting to encode a String which is too large for bootstring.")
                  } else {
                    (buffer, codePoint + 1, delta + 1, h, bias)
                  }
              }
          }
      } match {
        case (buffer, _, _, _, _) =>
          val pos: Int = buffer.position
          buffer.flip
          Right(new String(buffer.array, 0, pos))
      }
    } catch {
      case e: Exception =>
        Left(e.getLocalizedMessage)
    }
  }

  /**
   * Bootstring decode the given `String`.
   */
  def decodeRaw(
      params: BootstringParams
  )(
      value: String
  ): Either[String, String] = {
    type Index = Int

    // Insert a value into an `IntBuffer` at an index. If the `IntBuffer`
    // already has a value at the index, shift all values from the index to
    // position to the right.
    def insertAt(buffer: IntBuffer, index: Int, value: Int): IntBuffer = {
      val pos: Int = buffer.position()
      if (index >= pos) {
        position(maybeResize(buffer, index - buffer.remaining + 1).put(index, value), pos + 1)
      } else {
        // shift everything at the current index forward.
        position(
          put(maybeResize(buffer, 1))(index + 1, buffer, index, pos - index).put(index, value),
          pos + 1)
      }
    }

    @tailrec
    def decodeCodePointAndIndex(
        k: Int,
        nonBasicDeltas: List[Int],
        i: Int,
        w: Int,
        bias: Bias
    ): (Int, List[Int]) =
      nonBasicDeltas match {
        case x :: xs =>
          val digit: Int = params.base.unsafeCodePointDigitToInt(x)
          Math.addExact(i, Math.multiplyExact(digit, w)) match {
            // Intentional shadow
            case i =>
              val threshold: Int = if (k <= bias.value + params.tmin.value) {
                params.tmin.value
              } else if (k >= bias.value + params.tmax.value) {
                params.tmax.value
              } else {
                k - bias.value
              }

              if (digit < threshold) {
                (i, xs)
              } else {
                decodeCodePointAndIndex(
                  k = k + params.base.value,
                  nonBasicDeltas = xs,
                  i = i,
                  w = Math.multiplyExact(w, Math.subtractExact(params.base.value, threshold)),
                  bias = bias)
              }
          }
        case Nil =>
          throw new RuntimeException(
            "Reached end of input in incomplete decoding state. This is not a validly encoded Bootstring string.")
      }

    @tailrec
    def decodeNext(
        nonBasicDeltas: List[Int],
        oldI: Int,
        bias: Bias,
        n: Int,
        outputLength: Int,
        acc: IntBuffer
    ): String =
      nonBasicDeltas match {
        case Nil =>
          val pos: Int = acc.position()
          acc.flip
          new String(acc.array, 0, pos)
        case xs =>
          // decodeCodePointAndIndex consumes the acc and is the reductive step.
          decodeCodePointAndIndex(
            k = params.base.value,
            nonBasicDeltas = xs,
            i = oldI,
            w = 1,
            bias = bias) match {
            // Intentional Shadow
            case (i, xs) =>
              val nextOutputLength: Int = outputLength + 1
              val nextBias: Bias =
                params.unsafeAdaptBias(i - oldI, nextOutputLength, oldI === 0)
              val nextN: Int = Math.addExact(n, i / nextOutputLength)
              i % nextOutputLength match {
                // Intentional Shadow
                case i =>
                  insertAt(acc, i, nextN)
                  if (params.isBasicCodePoint(nextN) === false) {
                    i % nextOutputLength match {
                      // Intentional Shadow
                      case i =>
                        decodeNext(
                          nonBasicDeltas = xs,
                          oldI = i + 1,
                          bias = nextBias,
                          n = nextN,
                          nextOutputLength,
                          acc = acc
                        )
                    }
                  } else {
                    throw new RuntimeException(
                      s"Decoded code point is basic. This is invalid for Bootstring decoding. Code Point: ${nextN}")
                  }
              }
          }
      }

    @tailrec
    def doOutput(
        basicCodePoints: List[Int],
        nonBasicCodePointMap: SortedMap[Index, Int],
        out: IntBuffer
    ): String =
      nonBasicCodePointMap.headOption match {
        case Some((i, cp)) if i === out.position =>
          doOutput(basicCodePoints, nonBasicCodePointMap - i, out.put(cp))
        case _ =>
          basicCodePoints match {
            case x :: xs =>
              doOutput(xs, nonBasicCodePointMap, out.put(x))
            case _ =>
              nonBasicCodePointMap
                .headOption
                .fold {
                  val pos: Int = out.position
                  out.flip
                  new String(out.array, 0, pos)
                } {
                  case (pos, _) =>
                    throw new RuntimeException(
                      s"Exhausted basic code points at index ${out.position()}, but we still have ${nonBasicCodePointMap.size} non basic code points to encode (next index is at ${pos}).")
                }
          }
      }

    try {
      // Split out the basic and non-basic sections of the encoded string.

      // Partitioning has to be done in terms of _code points_, not
      // java.lang.Char. While not common, there is no restriction in
      // Bootstring to prevent the delimiter from being a code point which
      // would be represented by a surrogate pair in UTF-16, thus the more
      // simple value.span(_ != params.delimiter.value.toChar) would be
      // invalid.
      val (basicCodePoints, nonBasicCodePointDeltas, basicCodePointLength, _)
          : (List[Int], List[Int], Int, Boolean) =
        foldLeftCodePoints(value.reverse)((List.empty[Int], List.empty[Int], 0, false)) {
          case ((Nil, nonBasic, basicCodePointLength, false), cp) =>
            if (cp === params.delimiter.codePoint) {
              (Nil, nonBasic, basicCodePointLength, true)
            } else {
              (Nil, cp +: nonBasic, basicCodePointLength, false)
            }
          case ((basic, nonBasic, basicCodePointLength, flag), cp) =>
            // Once we found the last instance of the delimiter (remember we are
            // traversing in reverse here), then all the other code points are
            // basic.
            if (params.isBasicCodePoint(cp)) {
              (cp +: basic, nonBasic, basicCodePointLength + 1, flag)
            } else {
              throw new RuntimeException(
                s"Encountered non-basic codepoint win the basic only code point region of the Bootstring encoded string. This means this not a properly encoded Bootstring value. Code Point: ${cp}.")
            }
        }

      // Output size will usually be _smaller_ than the input, but if it is an
      // unusual Bootstring encoding, then it could be using characters which
      // are surrogate pairs in the _encoded_ domain. In that case, output size
      // might be large than the input, but by no more than a factor of 2.
      val out: IntBuffer = IntBuffer.allocate(value.size * 2)

      basicCodePoints.foreach(cp => out.put(cp))

      Right(
        decodeNext(
          nonBasicDeltas = nonBasicCodePointDeltas,
          oldI = 0,
          bias = params.initialBias,
          n = params.initialN,
          outputLength = basicCodePointLength,
          acc = out
        )
      )
    } catch {
      case e: Exception =>
        Left(e.getLocalizedMessage)
    }
  }
}
