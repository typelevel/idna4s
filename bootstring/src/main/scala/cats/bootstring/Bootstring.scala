package  cats.bootstring

import scala.annotation.tailrec
import cats._
import cats.syntax.all._
import java.nio.IntBuffer
import java.nio.ByteBuffer

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
    val out: IntBuffer = ???
    val codePointIndexOfLastDelimiter: Option[Int] = lastIndexOf(in, params.delimiter)

    @tailrec
    def baseLoop(k: Int, i: Int, w: Int, bias: Bias): (Int, Int) =
      if (in.hasRemaining) {
        val codePoint: Int = in.get
        val digit: Int = params.base.unsafeDecodeDigit(codePoint)
        val nexti: Int =
          java.lang.Math.addExact(i, java.lang.Math.multiplyExact(digit, w))
        val t: Int =
          if (k <= bias.value + params.tmin.value) {
            params.tmin.value
          } else if (k >= bias.value + params.tmax.value) {
            params.tmax.value
          } else {
            k - bias.value
          }
        if (digit < t) {
          (nexti, w)
        } else {
          val nextw: Int =
            java.lang.Math.multiplyExact(w, java.lang.Math.addExact(params.base.value, -t))
          baseLoop(k = k + params.base.value, i = nexti, w = nextw, bias)
        }
      } else {
        throw new RuntimeException(s"Expected additional input, but input was empty.")
      }

    @tailrec
    def processRest(n: Int, i: Int, bias: Bias, outLen: Int): String =
      if (in.hasRemaining) {
        val (nexti, nextw): (Int, Int) = baseLoop(k = params.base.value, i = i, w = 1, bias = bias)
        val nextOutLen: Int = java.lang.Math.addExact(outLen, 1)
        val nextBias: Bias = bias.unsafeAdapt(
          params
        )(
          delta = nexti - i,
          numpoints = nextOutLen,
          firstTime = i === 0
        )
        val codePoint: Int = java.lang.Math.addExact(n, java.lang.Math.divideExact(nexti, nextOutLen))
        val pos: Int = nexti % nextOutLen
        if (params.basicCodepoints.contains(codePoint)) {
          throw new RuntimeException(s"Encountered basic code point when only extended codepoint points are expected: ${codePoint}")
        } else {
          out.put(pos, codePoint)
          processRest(codePoint, pos + 1, nextBias, nextOutLen)
        }
      } else {
        out.flip
        new String(out.array(), 0, outLen)
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

  // private[this] def encode[F[_]](
  //   initialN: Long,
  //   initialBias: Long,
  //   basicCodepointPred: Int => Boolean
  // )(
  //   input: CharSequence
  // ): String =

}
