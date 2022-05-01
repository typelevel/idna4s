package  cats.bootstring

import scala.annotation.tailrec
import cats._
import cats.data._
import cats.syntax.all._
import java.nio.IntBuffer
import java.nio.ByteBuffer
import scala.collection.immutable.SortedSet
import scala.collection.immutable.SortedMap
import java.lang.Math

object Bootstring {

  @inline
  private def insertCodePoint(buffer: IntBuffer, codePoint: Int): IntBuffer =
    if (buffer.hasRemaining) {
      buffer.put(codePoint)
    } else {
      IntBuffer.allocate(buffer.limit * 2).put(buffer.array).put(codePoint)
    }

  @inline
  private def insertAt(buffer: IntBuffer, index: Int, value: Int): IntBuffer = {
    val pos: Int = buffer.position()
    if (index >= pos) {
      buffer.put(index, value).position(index + 1)
    } else {
      // shift everything at the current index forward
      buffer.put(index + 1, buffer, index, pos - index).put(index, value).position(pos + 1)
    }
  }

  def encode(
    params: BootstringParams
  )(
    value: String
  ): Either[String, String] = {

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
        insertCodePoint(buffer, params.base.unsafeDigitToCodePoint(q))
      } else {
        val qt: Int = q - threshold
        val bt: Int = params.base.value - threshold
        encodeCodePoint(
          insertCodePoint(buffer, params.base.unsafeDigitToCodePoint(threshold + (qt % bt))),
          bias,
          q = qt/bt,
          k = k + params.base.value
        )
      }
    }

    try {
    val (basicCodePoints, nonBasicCodePoints) = foldLeftCodePoints(value)((IntBuffer.allocate(value.length * 2), SortedSet.empty[Int])){
      case ((buffer, nonBasic), codePoint) =>
        if (params.isBasicCodePoint(codePoint)) {
          (buffer.put(codePoint), nonBasic)
        } else if (codePoint < params.initialN) {
          // Only occurs in unusual BootString usage.
          throw new RuntimeException(s"Input contains a non-basic code point < the initial N value. Code Point: ${codePoint}, Initial N: ${params.initialN}.")
        } else {
          (buffer, nonBasic + codePoint)
        }
    }
    val basicCodePointCount: Int = basicCodePoints.position()

    // Insert the delimiter if there is at least one basic code point
    if (basicCodePointCount =!= 0) {
      basicCodePoints.put(params.delimiter.codePoint)
    }

    nonBasicCodePoints.foldLeft((basicCodePoints, params.initialN, 0, basicCodePointCount, params.initialBias)){
      case ((buffer, previousCodePoint, delta, h, bias), codePoint) =>
        Math.addExact(delta, Math.multiplyExact(codePoint - previousCodePoint, Math.addExact(h, 1))) match {
          case delta =>
            foldLeftCodePoints(value)((buffer, bias, delta, h)){
              case ((buffer, bias, delta, h), cp) =>
                if (cp < codePoint) {
                  (buffer, bias, delta + 1, h)
                } else if (cp === codePoint) {
                  (encodeCodePoint(buffer, bias, delta, params.base.value), bias.unsafeAdapt(params)(delta, h + 1, h === basicCodePointCount), 0, h + 1)
                } else {
                  (buffer, bias, delta, h)
                }
            } match {
              case (buffer, bias, delta, h) =>
                if (delta === Int.MaxValue) {
                  throw new RuntimeException("Delta will overflow if encoding continues, this probably means you are attempting to encode a String which is too large for bootstring.")
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

  def decode(
    params: BootstringParams
  )(
    value: String
  ): Either[String, String] = {
    type N = Int
    type Index = Int

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
          val digit: Int = params.base.unsafeCodePointToDigit(x)
          println(s"k = $k, i = $i, w = $w, bias = $bias, x = $x, digit = $digit")
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
                decodeCodePointAndIndex(k = k + params.base.value, nonBasicDeltas = xs, i = i, w = Math.multiplyExact(w, Math.subtractExact(params.base.value, threshold)), bias = bias)
              }
          }
        case Nil =>
          throw new RuntimeException("Reached end of input in incomplete decoding state. This is not a validly encoded Bootstring string.")
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
          decodeCodePointAndIndex(k = params.base.value, nonBasicDeltas = xs, i = oldI, w = 1, bias = bias) match {
            // Intentional Shadow
            case (i, xs) =>
              val nextOutputLength: Int = outputLength + 1
              val nextBias: Bias = bias.unsafeAdapt(params)(i - oldI, nextOutputLength, oldI === 0)
              val nextN: Int = Math.addExact(n, Math.divideExact(i, nextOutputLength))
              i % nextOutputLength match {
                //Intentional Shadow
                case i =>
                  insertAt(acc, i, nextN)
                  println(s"nextN: $nextN, bias: $nextBias, position: ${i}, nextOutputLength: $nextOutputLength")
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
                    throw new RuntimeException(s"Decoded code point is basic. This is invalid for Bootstring decoding. Code Point: ${nextN}")
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
              nonBasicCodePointMap.headOption.fold{
                val pos: Int = out.position
                out.flip
                new String(out.array, 0, pos)
              }{
                case (pos, _) =>
                  throw new RuntimeException(s"Exhausted basic code points at index ${out.position()}, but we still have ${nonBasicCodePointMap.size} non basic code points to encode (next index is at ${pos}).")
              }
          }
      }

    try {
      // Split out the basic and non-basic sections of the encoded string.

      // Partitioning has to be done in terms of _code points_, not
      // java.lang.Char. While not common, there is no restriction in
      // Bootstring to prevent the delimiter from being a surrogate pair, thus
      // the more simple value.span(_ != params.delimiter.value.toChar) would
      // be invalid.
      val (basicCodePoints, nonBasicCodePointDeltas, basicCodePointLength, _): (List[Int], List[Int], Int, Boolean) =
        foldLeftCodePoints(value.reverse)((List.empty[Int], List.empty[Int], 0, false)){
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
              throw new RuntimeException(s"Encountered non-basic codepoint win the basic only code point region of the Bootstring encoded string. This means this not a properly encoded Bootstring value. Code Point: ${cp}.")
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
