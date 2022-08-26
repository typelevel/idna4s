/*
 * Copyright 2022 Typelevel
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.typelevel.idna4s.bootstring

import cats.data._
import cats.syntax.all._

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
}
