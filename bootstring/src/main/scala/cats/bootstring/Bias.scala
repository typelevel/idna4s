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

import cats._
import cats.syntax.all._
import scala.annotation.tailrec

sealed abstract class Bias extends Product with Serializable {
  def value: Int

  final def adapt(damp: Damp, base: Base, tmin: TMin, tmax: TMax, skew: Skew)(
      delta: Int,
      numpoints: Int,
      firstTime: Boolean): Either[String, Bias] =
    Bias.adapt(damp, base, tmin, tmax, skew)(
      bias = this,
      delta = delta,
      numpoints = numpoints,
      firstTime = firstTime)

  final def adapt(bootstringParams: BootstringParams)(
      delta: Int,
      numpoints: Int,
      firstTime: Boolean): Either[String, Bias] =
    Bias.adapt(bootstringParams)(
      bias = this,
      delta = delta,
      numpoints = numpoints,
      firstTime = firstTime)

  final def unsafeAdapt(bootstringParams: BootstringParams)(
      delta: Int,
      numpoints: Int,
      firstTime: Boolean): Bias =
    Bias.unsafeAdapt(bootstringParams)(
      bias = this,
      delta = delta,
      numpoints = numpoints,
      firstTime = firstTime)

  final def unsafeAdapt(damp: Damp, base: Base, tmin: TMin, tmax: TMax, skew: Skew)(
      delta: Int,
      numpoints: Int,
      firstTime: Boolean): Bias =
    Bias.unsafeAdapt(damp, base, tmin, tmax, skew)(
      bias = this,
      delta = delta,
      numpoints = numpoints,
      firstTime = firstTime)

  final override def toString: String = s"Bias(value = ${value})"
}

object Bias {
  final private[this] case class BiasImpl(override val value: Int) extends Bias

  val PunycodeInitialBias: Bias = unsafeInitialFromInt(72)

  def adapt(bootstringParams: BootstringParams)(
      bias: Bias,
      delta: Int,
      numpoints: Int,
      firstTime: Boolean): Either[String, Bias] =
    adapt(
      damp = bootstringParams.damp,
      base = bootstringParams.base,
      tmin = bootstringParams.tmin,
      tmax = bootstringParams.tmax,
      skew = bootstringParams.skew
    )(
      bias = bias,
      delta = delta,
      numpoints = numpoints,
      firstTime = firstTime
    )

  def adapt(damp: Damp, base: Base, tmin: TMin, tmax: TMax, skew: Skew)(
      bias: Bias,
      delta: Int,
      numpoints: Int,
      firstTime: Boolean): Either[String, Bias] = {

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

      Right(BiasImpl(k + (((base.value - tmin.value + 1) * delta2) / (delta2 + skew.value))))
    } else {
      Left(
        s"The number of encoded/decoded codepoints must be > 0 when adapting the bias, but was ${numpoints}")
    }
  }

  def unsafeAdapt(bootstringParams: BootstringParams)(
      bias: Bias,
      delta: Int,
      numpoints: Int,
      firstTime: Boolean): Bias =
    adapt(bootstringParams)(
      bias = bias,
      delta = delta,
      numpoints = numpoints,
      firstTime = firstTime).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )

  def unsafeAdapt(damp: Damp, base: Base, tmin: TMin, tmax: TMax, skew: Skew)(
      bias: Bias,
      delta: Int,
      numpoints: Int,
      firstTime: Boolean): Bias =
    adapt(damp, base, tmin, tmax, skew)(
      bias = bias,
      delta = delta,
      numpoints = numpoints,
      firstTime = firstTime).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )

  def initialFromInt(value: Int): Either[String, Bias] =
    if (value >= 0) {
      Right(BiasImpl(value))
    } else {
      Left(s"Initial Bias may not be < 0.")
    }

  def unsafeInitialFromInt(value: Int): Bias =
    initialFromInt(value).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )

  def fromString(value: String): Either[String, Bias] =
    ApplicativeError[Either[Throwable, *], Throwable]
      .catchNonFatal(
        value.toInt
      )
      .leftMap(_.getLocalizedMessage)
      .flatMap(initialFromInt)

  def unsafeFromString(value: String): Bias =
    fromString(value).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )
}
