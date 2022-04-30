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
    val in: IntBuffer = codePointsAsBuffer(value)
    val out: IntBuffer = IntBuffer.allocate(in.capacity)
    val codePointIndexOfLastDelimiter: Option[Int] = lastIndexOf(in, params.delimiter)

    @tailrec
    def baseLoop(k: Int, i: Int, w: Int, bias: Bias): Int = {
      println(s"\nbaseLoop(k = $k, i = $i, w = $w, bias = $bias)")
      if (in.hasRemaining) {
        val codePoint: Int = in.get
        println(s"Consumed: ${new String(Character.toChars(codePoint))}")
        val digit: Int = params.base.unsafeDigitToCodePoint(codePoint)
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
        if (params.isBasicCodePoint(codePoint)) {
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

  val testString: String =
    "\u0033\u5E74\u0042\u7D44\u91D1\u516B\u5148\u751F"

  @inline
  private def insertCodePoint(buffer: IntBuffer, codePoint: Int): IntBuffer =
    if (buffer.hasRemaining) {
      buffer.put(codePoint)
    } else {
      IntBuffer.allocate(buffer.limit * 2).put(buffer.array).put(codePoint)
    }

  def encode(
    params: BootstringParams
  )(
    value: String
  ) = {

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
        println(s"Terminal: $bias $q $k")
        insertCodePoint(buffer, params.base.unsafeDigitToCodePoint(q))
      } else {
        val qt: Int = q - threshold
        val bt: Int = params.base.value - threshold
        println(s"Loop: $bias $q $k")
        println(s"digit: ${threshold + (qt % bt)}")
        encodeCodePoint(
          insertCodePoint(buffer, params.base.unsafeDigitToCodePoint(threshold + (qt % bt))),
          bias,
          q = qt/bt,
          k = k + params.base.value
        )
      }
    }

    val (basicCodePoints, nonBasicCodePoints) = foldLeftCodePoints(value)((IntBuffer.allocate(value.length * 2), SortedSet.empty[Int])){
      case ((buffer, nonBasic), codePoint) =>
        if (params.isBasicCodePoint(codePoint)) {
          (buffer.put(codePoint), nonBasic)
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
      case ((buffer, previousCodePoint, delta, index, bias), codePoint) =>
        println(s"\nCodePoint: $codePoint")
        delta + (codePoint - previousCodePoint) * (index + 1) match {
          case delta =>
            foldLeftCodePoints(value)((buffer, bias, delta, index)){
              case ((buffer, bias, delta, index), cp) =>
                if (cp < codePoint) {
                  (buffer, bias, delta + 1, index)
                } else if (cp === codePoint) {
                  (encodeCodePoint(buffer, bias, delta, params.base.value), bias.unsafeAdapt(params)(delta, index + 1, index === basicCodePointCount), 0, index + 1)
                } else {
                  (buffer, bias, delta, index)
                }
            } match {
              case (buffer, bias, delta, index) =>
                (buffer, codePoint + 1, delta + 1, index, bias)
            }
        }
    } match {
      case (buffer, _, _, _, _) =>
        val pos: Int = buffer.position
        buffer.flip
        new String(buffer.array, 0, pos)
    }
  }

  def test = {
    val result = encode(BootstringParams.PunycodeParams)(testString)
    val correct = "3B-ww4c5e180e575a65lsy2b"
    (result, correct, result === correct)
  }

  // def encode(
  //   params: BootstringParams
  // )(
  //   value: String
  // ): Either[String, String] = {
  //   val in: IntBuffer = codePoints(value)
  //   val out: IntBuffer = ???

  //   @tailrec
  //   def copyBasicCodePointsToOutput(nonBasicCodePoints: SortedSet[Int], count: Int): (SortedSet[Int], Int) =
  //     if (in.hasRemaining) {
  //       val codePoint: Int = in.get
  //       if (params.basicCodepoints.contains(codePoint)) {
  //         out.put(codePoint)
  //         copyBasicCodePointsToOutput(nonBasicCodePoints, count + 1)
  //       } else if (codePoint >= params.initialN) {
  //         copyBasicCodePointsToOutput(nonBasicCodePoints + codePoint, count)
  //       } else {
  //         throw new RuntimeException(s"Input contains a non-basic codepoint < than the initial N value. This is not valid. CodePoint: ${codePoint}, Initial N: ${params.initialN}")
  //       }
  //     } else {
  //       in.rewind
  //       if (count > 0) {
  //         out.put(params.delimiter.codePoint)
  //         count
  //       } else {
  //         count
  //       }
  //     }

  //   @tailrec
  //   def innerLoop1(k: Int, bias: Bias, q: Int): Int = {
  //     val t: Int =
  //       if (k <= bias.value) {
  //         params.TMin.value
  //       } else if (k >= (bias.value + params.TMax.value)) {
  //         params.TMax.value
  //       } else {
  //         k - bias.value
  //       }

  //     if (q < t) {
  //       q
  //     } else {
  //       out.put(params.base.unsafeEncodeToDigitCodePoint(t + ((q - t) % (params.base.value - t))))
  //       innerLoop1(k = k + params.base.value, bias = bias, q = (q - t)/(params.base.value - t))
  //     }
  //   }

  //   @tailrec
  //   def innerLoop0(cursor: Int, delta: Int, n: Int, bias: Bias, h: Int, numberOfBasicCodePoints: Int): (Bias, Int) = {
  //     if (cursor > in.limit) {
  //       (bias, h)
  //     } else {
  //       val codePoint: Int = in.get(cursor)
  //       val nextDelta: Int =
  //         if (codePoint < n || params.basicCodepoints.contains(codePoint)) {
  //           Math.addExact(delta, 1)
  //         } else {
  //           delta
  //         }
  //       if (codePoint === n) {
  //         out.put(params.base.unsafeEncodeToDigitCodePoint(innerLoop1(k = params.base.value, bias = bias, nextDelta)))
  //         val nextBias: Bias =
  //           bias.adapt(params)(delta = nextDelta, numpoints = h + 1, h === numberOfBasicCodePoints)
  //         innerLoop0(cursor = cursor + 1, delta = 0, n = n, bias = nextBias, h = h + 1, numberOfBasicCodePoints = numberOfBasicCodePoints)
  //       } else {
  //         innerLoop0(cursor = cursor + 1, delta = nextDelta, n = n, bias = bias, h = h, numberOfBasicCodePoints = numberOfBasicCodePoints)
  //       }
  //     }
  //   }

  //   @tailrec
  //   def outerLoop(h: Int, delta: Int, n: Int, bias: Bias, nonBasicCodePoints: SortedSet[Int], numberOfBasicCodePoints: Int): String =
  //     if (h < in.limit) {
  //       nonBasicCodePoints.dropWhile(_ < n) match {
  //         case nonBasicCodePoints =>
  //           nonBasicCodePoints.headOption match {
  //             case Some(m) =>
  //               Math.addExact(delta, Math.multiplyExact(Math.subtractExact(m, n), Math.addExact(h, 1))) match {
  //                 case delta =>
  //                   val (nextBias, nextH) = innerLoop0(cursor = 0, delta = delta, n = m, bias = bias, h = h, numberOfBasicCodePoints)

  //               }
  //             case _ =>
  //               throw new RuntimeException(s"There are no more non-basic code points >= the current n value: ${n}")
  //           }
  //       }
  //     } else {
  //       val position: Int = out.position()
  //       out.flip
  //       new String(out.array, 0, position)
  //     }

  //   try {
  //     val basicCodePointCount: Int = copyBasicCodePointsToOutput(0)
  //   } catch {
  //     case e: Exception =>
  //       Left(e.getLocalizedMessage)
  //   }
  // }

  // def encode(
  //   params: BootstringParams
  // )(
  //   value: String
  // ): Either[String, String] = {
  //   import Chain._
  //   val in: Chain[Int] = codePointsAsChain(value)
  //   // Safe conversion because a String can only be up to Int.MaxValue and when we convert it to code points it can only be <= the String length.
  //   val len: Int = in.length.toInt

  //   @tailrec
  //   def outputBasicCodePoints(in: Chain[Int], basicCodePointCount: Int, nonBasicCodePointSet: SortedSet[Int], out: Chain[Int]): (SortedSet[Int], Int, Chain[Int]) =
  //     in match {
  //       case x ==: xs =>
  //         if (params.basicCodepoints.contains(x)) {
  //           outputBasicCodePoints(xs, basicCodePointCount + 1, nonBasicCodePointSet, out :+ x)
  //         } else if (x < params.initialN) {
  //           throw new RuntimeException(s"Intput contains a non-basic code point which is less than the initial N value. This is illegal. Code Point: ${x}, Initial N: ${params.initialN}.")
  //         } else {
  //           outputBasicCodePoints(xs, basicCodePointCount, nonBasicCodePointSet + x, out)
  //         }
  //       case _ =>
  //         if (basicCodePointCount > 0) {
  //           (nonBasicCodePointSet, basicCodePointCount, out :+ params.delimiter.codePoint)
  //         } else {
  //           (nonBasicCodePointSet, basicCodePointCount, out)
  //         }
  //     }

  //   @tailrec
  //   def encodeCodePoint(k: Int, bias: Bias, q: Int, out: Chain[Int]): Chain[Int] = {
  //     val t: Int = if (k <= bias.value) {
  //       params.tmin.value
  //     } else if (k >= bias.value + params.tmax.value) {
  //       params.tmax.value
  //     } else {
  //       k - bias.value
  //     }

  //     if (q < t) {
  //       out :+ params.base.unsafeEncodeToDigitCodePoint(q)
  //     } else {
  //       val qt: Int = q - t
  //       val bt: Int = params.base.value - t
  //       out :+ (t + (qt % bt))
  //       encodeCodePoint(k = k + params.base.value, bias = bias, q = qt / bt, out)
  //     }
  //   }

  //   try {
  //     outputBasicCodePoints(in, 0, SortedSet.empty[Int], Chain.empty[Int]) match {
  //       case (nonBasicCodePointSet, basicCodePointCount, out) =>
  //         nonBasicCodePointSet.foldLeft((params.initialN, 0, basicCodePointCount, params.initialBias, out)){
  //           case ((n, delta, inputCodePointsEmitted, bias, out), codePointLevel) =>
  //             if (n < codePointLevel) {
  //               // TODO: Remove once I'm sure this is correct.
  //               throw new AssertionError(s"n < codePointLevel. This should not be possible. ${n} < ${codePointLevel}.")
  //             } else {
  //               Math.addExact(delta, Math.multiplyExact(Math.subtractExact(codePointLevel, n), Math.addExact(inputCodePointsEmitted, 1))) match {
  //                 case delta =>
  //                     in.foldLeft((delta, bias, inputCodePointsEmitted, out)){
  //                       case ((delta, bias, inputCodePointsEmitted, out), codePoint) =>
  //                         if (codePoint < delta) {
  //                           (Math.addExact(delta, 1), bias, inputCodePointsEmitted, out)
  //                         } else if (codePoint > delta) {
  //                           // TODO: Remove once I'm sure this is correct.
  //                           throw new AssertionError(s"codePoint > delta, this should not be possible. $codePoint > $delta")
  //                         } else {
  //                           // codePoint == delta
  //                           encodeCodePoint(k = params.base.value, bias = bias, q = delta, out = out) match {
  //                             case out =>
  //                               (0, bias.unsafeAdapt(params)(delta = delta, numpoints = inputCodePointsEmitted + 1, firstTime = inputCodePointsEmitted === 0), inputCodePointsEmitted + 1, out)
  //                           }
  //                         }
  //                     } match {
  //                       case (delta, bias, inputCodePointsEmitted, out) =>
  //                         (n + 1, delta + 1, inputCodePointsEmitted, bias, out)
  //                     }
  //               }
  //             }
  //         }
  //     } match {
  //       case (_, _, emittedCodePoints, out) =>
  //         if (emittedCodePoints =!= len) {
  //           throw new AssertionError(s"emittedCodePoints =!= len: $emittedCodePoints =!= $len")
  //         } else {
  //           Right(new String(out.toList.toArray, 0, out.length))
  //         }
  //     }
  //   } catch {
  //     case e: Exception =>
  //       Left(e.getLocalizedMessage)
  //   }
  // }
}
