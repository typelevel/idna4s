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

import cats._
import cats.data._
import cats.syntax.all._
import scala.annotation.tailrec

sealed abstract class BootstringParams extends Product with Serializable {
  def isBasicCodePoint: Int => Boolean
  def base: Base
  def delimiter: Delimiter
  def tmin: TMin
  def tmax: TMax
  def skew: Skew
  def damp: Damp
  def initialBias: Bias
  def initialN: Int

  // final

  final def unsafeAdaptBias(
      delta: Int,
      numpoints: Int,
      firstTime: Boolean
  ): Bias =
    BootstringParams.unsafeAdaptBias(this)(
      delta = delta,
      numpoints = numpoints,
      firstTime = firstTime
    )

  final def adaptBias(
      delta: Int,
      numpoints: Int,
      firstTime: Boolean
  ): Either[String, Bias] =
    BootstringParams.adaptBias(this)(
      delta = delta,
      numpoints = numpoints,
      firstTime = firstTime
    )

  final override def toString: String =
    s"BootstringParams(base = ${base}, delimiter = ${delimiter}, tmin = ${tmin}, tmax = ${tmax}, skew = ${skew}, damp = ${damp}, initialBias = ${initialBias}, initialN = ${initialN})"

}

object BootstringParams {
  final private[this] case class BootstringParamsImpl(
      override val isBasicCodePoint: Int => Boolean,
      override val base: Base,
      override val delimiter: Delimiter,
      override val tmin: TMin,
      override val tmax: TMax,
      override val skew: Skew,
      override val damp: Damp,
      override val initialBias: Bias,
      override val initialN: Int
  ) extends BootstringParams

  val PunycodeParams: BootstringParams =
    unsafeFrom(
      0x7f,
      Base.PunycodeBase,
      Delimiter.PunycodeDelimiter,
      TMin.PunycodeTMin,
      TMax.PunycodeTMax,
      Skew.PunycodeSkew,
      Damp.PunycodeDamp,
      Bias.PunycodeInitialBias
    )

  def apply(
      isBasicCodePoint: Int => Boolean,
      base: Base,
      delimiter: Delimiter,
      tmin: TMin,
      tmax: TMax,
      skew: Skew,
      damp: Damp,
      initialBias: Bias,
      initialN: Int
  ): Either[NonEmptyList[String], BootstringParams] =
    (
      (s"tmin must be <= tmax: ${tmin} > ${tmax}".leftNel[Unit]).whenA(tmin.value > tmax.value),
      (s"initial_bias mod base must be <= base - tmin: ${initialBias} mod ${base} > ${base} - ${tmin}"
        .leftNel[Unit])
        .whenA(initialBias.value % base.value > (base.value - tmin.value))).parMapN {
      case _ =>
        BootstringParamsImpl(
          isBasicCodePoint,
          base,
          delimiter,
          tmin,
          tmax,
          skew,
          damp,
          initialBias,
          initialN
        )
    }

  def apply(
      maxBasicCodePoint: Int,
      base: Base,
      delimiter: Delimiter,
      tmin: TMin,
      tmax: TMax,
      skew: Skew,
      damp: Damp,
      initialBias: Bias
  ): Either[NonEmptyList[String], BootstringParams] =
    if (maxBasicCodePoint < Character.MAX_CODE_POINT && maxBasicCodePoint >= 0) {
      apply(
        (codePoint: Int) => codePoint <= maxBasicCodePoint && codePoint >= 0,
        base,
        delimiter,
        tmin,
        tmax,
        skew,
        damp,
        initialBias,
        maxBasicCodePoint + 1
      )
    } else {
      s"The maximum basic code point must be >= 0 and < Character.MAX_CODE_POINT: ${maxBasicCodePoint}"
        .leftNel[BootstringParams]
    }

  def unsafeFrom(
      isBasicCodePoint: Int => Boolean,
      base: Base,
      delimiter: Delimiter,
      tmin: TMin,
      tmax: TMax,
      skew: Skew,
      damp: Damp,
      initialBias: Bias,
      initialN: Int
  ): BootstringParams =
    apply(
      isBasicCodePoint,
      base,
      delimiter,
      tmin,
      tmax,
      skew,
      damp,
      initialBias,
      initialN
    ).fold(
      errors =>
        throw new IllegalArgumentException(
          s"""Error(s) encountered when building BootstringParams: ${errors.mkString_(
            ", ")}"""),
      identity
    )

  def unsafeFrom(
      maxBasicCodePoint: Int,
      base: Base,
      delimiter: Delimiter,
      tmin: TMin,
      tmax: TMax,
      skew: Skew,
      damp: Damp,
      initialBias: Bias
  ): BootstringParams =
    apply(
      maxBasicCodePoint,
      base,
      delimiter,
      tmin,
      tmax,
      skew,
      damp,
      initialBias
    ).fold(
      errors =>
        throw new IllegalArgumentException(
          s"""Error(s) encountered when building BootstringParams: ${errors.mkString_(
            ", ")}"""),
      identity
    )

  // Bias adaptation functions
  //
  // These are found in this file, rather than in companion object for Bias,
  // because bias adaptation is a function of the bootstring parameters and
  // the bootstring state, but _not_ the current bias.

  def unsafeAdaptBias(damp: Damp, base: Base, tmin: TMin, tmax: TMax, skew: Skew)(
      delta: Int,
      numpoints: Int,
      firstTime: Boolean): Bias = {

    @tailrec
    def loop(delta: Int, k: Int): (Int, Int) =
      if (delta > ((base.value - tmin.value) * tmax.value) / 2) {
        loop(delta / (base.value - tmin.value), k + base.value)
      } else {
        (delta, k)
      }

    if (numpoints > 0) {
      val delta0: Int =
        if (firstTime) {
          delta / damp.value
        } else {
          delta / 2
        }

      val delta1: Int =
        delta0 + (delta0 / numpoints)

      val (delta2, k) = loop(delta1, 0)

      Bias.unsafeFromInt(k + (((base.value - tmin.value + 1) * delta2) / (delta2 + skew.value)))
    } else {
      throw new IllegalArgumentException(
        s"The number of encoded/decoded codepoints must be > 0 when adapting the bias, but was ${numpoints}")
    }
  }

  @inline
  def unsafeAdaptBias(bootstringParams: BootstringParams)(
      delta: Int,
      numpoints: Int,
      firstTime: Boolean): Bias =
    unsafeAdaptBias(
      damp = bootstringParams.damp,
      base = bootstringParams.base,
      tmin = bootstringParams.tmin,
      tmax = bootstringParams.tmax,
      skew = bootstringParams.skew
    )(
      delta = delta,
      numpoints = numpoints,
      firstTime = firstTime
    )

  def adaptBias(bootstringParams: BootstringParams)(
      delta: Int,
      numpoints: Int,
      firstTime: Boolean): Either[String, Bias] =
    adaptBias(
      damp = bootstringParams.damp,
      base = bootstringParams.base,
      tmin = bootstringParams.tmin,
      tmax = bootstringParams.tmax,
      skew = bootstringParams.skew
    )(
      delta = delta,
      numpoints = numpoints,
      firstTime = firstTime
    )

  def adaptBias(damp: Damp, base: Base, tmin: TMin, tmax: TMax, skew: Skew)(
      delta: Int,
      numpoints: Int,
      firstTime: Boolean): Either[String, Bias] =
    ApplicativeError[Either[Throwable, *], Throwable]
      .catchNonFatal(
        unsafeAdaptBias(damp, base, tmin, tmax, skew)(
          delta = delta,
          numpoints = numpoints,
          firstTime = firstTime
        )
      )
      .leftMap(_.getLocalizedMessage)
}
