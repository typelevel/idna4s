package cats.bootstring

import cats.data._
import cats.syntax.all._
import scala.collection.immutable.BitSet

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

  override final def toString: String =
    s"BootstringParams(base = ${base}, delimiter = ${delimiter}, tmin = ${tmin}, tmax = ${tmax}, skew = ${skew}, damp = ${damp}, initialBias = ${initialBias}, initialN = ${initialN})"


}

object BootstringParams {
  private[this] final case class BootstringParamsImpl(
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
      0x7F,
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
    ((s"tmin must be <= tmax: ${tmin} > ${tmax}".leftNel[Unit]).whenA(tmin.value > tmax.value),
  (s"initial_bias mod base must be <= base - tmin: ${initialBias} mod ${base} > ${base} - ${tmin}".leftNel[Unit]).whenA(initialBias.value % base.value > (base.value - tmin.value))
    ).parMapN{
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
      s"The maximum basic code point must be >= 0 and < Character.MAX_CODE_POINT: ${maxBasicCodePoint}".leftNel[BootstringParams]
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
      errors => throw new IllegalArgumentException(s"""Error(s) encountered when building BootstringParams: ${errors.mkString_(", ")}"""),
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
      errors => throw new IllegalArgumentException(s"""Error(s) encountered when building BootstringParams: ${errors.mkString_(", ")}"""),
      identity
    )
}
