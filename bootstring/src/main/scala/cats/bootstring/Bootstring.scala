package cats.bootstring

import scala.annotation.tailrec
import cats._

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

  // private[this] def encode[F[_]](
  //   initialN: Long,
  //   initialBias: Long,
  //   basicCodepointPred: Int => Boolean
  // )(
  //   input: CharSequence
  // ): String =


}
