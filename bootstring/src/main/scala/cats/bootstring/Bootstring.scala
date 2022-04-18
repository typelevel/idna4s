package  cats.bootstring

import scala.annotation.tailrec
import cats._
import cats.syntax.all._
import java.nio.IntBuffer
import java.nio.ByteBuffer
import scala.collection.immutable.SortedSet
import java.lang.Math

object Bootstring {

  def basicCodepointsInCharSeq(
    basicCodepointPred: Int => Boolean,
    charSeq: CharSequence
  ): Long = {

    val size: Int = charSeq.length

    @tailrec
    def loop(index: Int, acc: Long): Long =
      if (index >= size) {
        acc
      } else {
        val codePoint: Int = Character.codePointAt(charSeq, index)
        val indexIncrement: Int = if (codePoint >= 0x10000) 2 else 1
        val nextAcc: Long =
          if (basicCodepointPred(codePoint)) {
            acc + 1L
          } else {
            acc
          }
        loop(index + indexIncrement, nextAcc)
      }

    loop(0, 0L)
  }

  def decode(
    params: BootstringParams
  )(
    value: String
  ): Either[String, String] = {
    val in: IntBuffer = codePoints(value)
    val out: IntBuffer = IntBuffer.allocate(in.capacity)
    val codePointIndexOfLastDelimiter: Option[Int] = lastIndexOf(in, params.delimiter)

    @tailrec
    def baseLoop(k: Int, i: Int, w: Int, bias: Bias): Int = {
      println(s"\nbaseLoop(k = $k, i = $i, w = $w, bias = $bias)")
      if (in.hasRemaining) {
        val codePoint: Int = in.get
        println(s"Consumed: ${new String(Character.toChars(codePoint))}")
        val digit: Int = params.base.unsafeDecodeDigit(codePoint)
        println(s"Digit: $digit")
        val nexti: Int =
          Math.addExact(i, Math.multiplyExact(digit, w))
        println(s"Next I: ${nexti}")
        val t: Int =
          if (k <= bias.value) {
            params.tmin.value
          } else if (k >= bias.value + params.tmax.value) {
            params.tmax.value
          } else {
            k - bias.value
          }
        if (digit < t) {
          nexti
        } else {
          val nextw: Int =
            Math.multiplyExact(w, Math.subtractExact(params.base.value, t))
          baseLoop(k = k + params.base.value, i = nexti, w = nextw, bias)
        }
      } else {
        throw new RuntimeException(s"Expected additional input, but input was empty.")
      }
    }

    @tailrec
    def processRest(n: Int, i: Int, bias: Bias, outLen: Int): String = {
      println(s"\nprocessRest($n, $i, $bias, $outLen)")
      if (in.hasRemaining) {
        val nexti: Int = baseLoop(k = params.base.value, i = i, w = 1, bias = bias)
        val nextOutLen: Int = Math.addExact(outLen, 1)
        val nextBias: Bias = bias.unsafeAdapt(
          params
        )(
          delta = nexti - i,
          numpoints = nextOutLen,
          firstTime = i === 0
        )
        val codePoint: Int = Math.addExact(n, Math.divideExact(nexti, nextOutLen))
        val pos: Int = nexti % nextOutLen
        if (params.basicCodepoints.contains(codePoint)) {
          throw new RuntimeException(s"Encountered basic code point when only extended code points are expected: ${codePoint}")
        } else {
          out.put(pos, codePoint)
          processRest(codePoint, pos + 1, nextBias, nextOutLen)
        }
      } else {
        out.flip
        new String(out.array(), 0, outLen)
      }
    }

    try {
      Right(codePointIndexOfLastDelimiter.fold(
        processRest(params.initialN, 0, params.initialBias, 0)
      ){codePointIndexOfLastDelimiter =>
        out.put(in.array(), 0, codePointIndexOfLastDelimiter)
        in.position(codePointIndexOfLastDelimiter + 1)
        processRest(params.initialN, 0, params.initialBias, codePointIndexOfLastDelimiter)
      })
    } catch {
      case e: Exception =>
        Left(e.getLocalizedMessage)
    }
  }

  def encode(
    params: BootstringParams
  )(
    value: String
  ): Either[String, String] = {
    val in: IntBuffer = codePoints(value)
    val out: IntBuffer = ???

    @tailrec
    def copyBasicCodePointsToOutput(nonBasicCodePoints: SortedSet[Int], count: Int): (SortedSet[Int], Int) =
      if (in.hasRemaining) {
        val codePoint: Int = in.get
        if (params.basicCodepoints.contains(codePoint)) {
          out.put(codePoint)
          copyBasicCodePointsToOutput(nonBasicCodePoints, count + 1)
        } else if (codePoint >= params.initialN) {
          copyBasicCodePointsToOutput(nonBasicCodePoints + codePoint, count)
        } else {
          throw new RuntimeException(s"Input contains a non-basic codepoint < than the initial N value. This is not valid. CodePoint: ${codePoint}, Initial N: ${params.initialN}")
        }
      } else {
        in.rewind
        if (count > 0) {
          out.put(params.delimiter.codePoint)
          count
        } else {
          count
        }
      }

    @tailrec
    def innerLoop1(k: Int, bias: Bias, q: Int): Int = {
      val t: Int =
        if (k <= bias.value) {
          params.TMin.value
        } else if (k >= (bias.value + params.TMax.value)) {
          params.TMax.value
        } else {
          k - bias.value
        }

      if (q < t) {
        q
      } else {
        out.put(params.base.unsafeEncodeToDigitCodePoint(t + ((q - t) % (params.base.value - t))))
        innerLoop1(k = k + params.base.value, bias = bias, q = (q - t)/(params.base.value - t))
      }
    }

    @tailrec
    def innerLoop0(cursor: Int, delta: Int, n: Int, bias: Bias, h: Int, numberOfBasicCodePoints: Int): (Bias, Int) = {
      if (cursor > in.limit) {
        (bias, h)
      } else {
        val codePoint: Int = in.get(cursor)
        val nextDelta: Int =
          if (codePoint < n || params.basicCodepoints.contains(codePoint)) {
            Math.addExact(delta, 1)
          } else {
            delta
          }
        if (codePoint === n) {
          out.put(params.base.unsafeEncodeToDigitCodePoint(innerLoop1(k = params.base.value, bias = bias, nextDelta)))
          val nextBias: Bias =
            bias.adapt(params)(delta = nextDelta, numpoints = h + 1, h === numberOfBasicCodePoints)
          innerLoop0(cursor = cursor + 1, delta = 0, n = n, bias = nextBias, h = h + 1, numberOfBasicCodePoints = numberOfBasicCodePoints)
        } else {
          innerLoop0(cursor = cursor + 1, delta = nextDelta, n = n, bias = bias, h = h, numberOfBasicCodePoints = numberOfBasicCodePoints)
        }
      }
    }

    @tailrec
    def outerLoop(h: Int, delta: Int, n: Int, bias: Bias, nonBasicCodePoints: SortedSet[Int], numberOfBasicCodePoints: Int): String =
      if (h < in.limit) {
        nonBasicCodePoints.dropWhile(_ < n) match {
          case nonBasicCodePoints =>
            nonBasicCodePoints.headOption match {
              case Some(m) =>
                Math.addExact(delta, Math.multiplyExact(Math.subtractExact(m, n), Math.addExact(h, 1))) match {
                  case delta =>
                    val (nextBias, nextH) = innerLoop0(cursor = 0, delta = delta, n = m, bias = bias, h = h, numberOfBasicCodePoints)

                }
              case _ =>
                throw new RuntimeException(s"There are no more non-basic code points >= the current n value: ${n}")
            }
        }
      } else {
        val position: Int = out.position()
        out.flip
        new String(out.array, 0, position)
      }

    try {
      val basicCodePointCount: Int = copyBasicCodePointsToOutput(0)
    } catch {
      case e: Exception =>
        Left(e.getLocalizedMessage)
    }
  }
}
