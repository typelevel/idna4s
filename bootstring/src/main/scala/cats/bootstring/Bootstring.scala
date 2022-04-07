package cats.bootstring

import scala.annotation.tailrec
import cats._
import cats.syntax.all._
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
    val out: ByteBuffer = ???
    val codePointIndexOfLastDelimiter: Option[Int] = lastIndexOf(in, delimiter)

    @tailrec
    def processBasicCodePoints(i: Int, codePointIndexOfLastDelimiter: Int): Unit =
      if (i >= codePointIndexOfLastDelimiter) {
        if (i == 0) {
          // Special case where there are no basic code points, don't consume
          // the delimiter.
          ()
        } else {
          // Clear delimiter
          in.get
          ()
        }
      } else {
        out.put(Character.toChars(in.get))
        processBasicCodePoints(i + 1, codePointIndexOfLastDelimiter)
      }

    @tailrec
    def consumeNonBasicCodePoints(i: Int, w: Int): Either[String, Unit] =
      if (in.hasRemaining) {
        val codePoint: Int = in.get
        base.decodeDigit(codePoint).map{digit =>
          val nexti: Int =
            i + digit * w

          if (nexti >= i) {
            val t: Int =

          } else {
            Left(s"Overflow during decoding.")
          }
        }
      } else {
        Left("Expected at least one more code point while decoding non-basic section, but found none.")
      }

    @tailrec
    def processRest(n: Long, bias: Long) =
      if (in.hasRemaining) {
        val oldi: Int = in.position
        val w: Int = 1
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
