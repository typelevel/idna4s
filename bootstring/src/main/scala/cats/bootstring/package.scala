package cats

import cats.data._
import java.nio.Buffer
import java.nio.CharBuffer
import java.nio.IntBuffer
import scala.annotation.tailrec

package object bootstring {
  def foldLeftCodePoints[A](value: String)(base: A)(f: (A, Int) => A): A = {
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

  def codePointsAsChain(value: String): Chain[Int] =
    foldLeftCodePoints(value)(Chain.empty[Int]){
      case (acc, value) =>
        acc :+ value
    }

  def codePointsAsBuffer(value: String): IntBuffer =
    foldLeftCodePoints(value)(IntBuffer.allocate(value.length)){
      case (acc, value) =>
        acc.put(value)
    }

  def lastIndexOf(value: IntBuffer, delimiter: Delimiter): Option[Int] = {

    @tailrec
    def loop(i: Int, out: Option[Int]): Option[Int] =
      if (i >= value.remaining) {
        out
      } else {
        if (value.get(i) == delimiter.codePoint) {
          loop(i + 1, Some(i))
        } else {
          loop(i + 1, out)
        }
      }

    loop(0, None)
  }

  def stringFromCodePoints(value: IntBuffer): Either[String, String] = {
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

  /** This method sets the position on a `Buffer`. This gets around a widening
    * in the Java 8 API.
    */
  def position[A <: Buffer](buffer: A, position: Int): A = {
    buffer.position(position) // This is a `Buffer` not `A` on JRE 8.
    buffer
  }

  /** This is an implementation of the Absolute bulk put operation added in JRE
    * 16 using methods available on JRE 8.
    */
  def put(buffer: IntBuffer)(index: Int, src: IntBuffer, offset: Int, length: Int): IntBuffer = {
    val oldPosition: Int = buffer.position()

    buffer.position(index)

    buffer.put(src.array, offset, length)

    buffer.position(oldPosition)

    buffer
  }
}
