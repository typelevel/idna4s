package cats.bootstring

import cats.data._
import cats.syntax.all._
import scala.collection.immutable.BitSet

sealed abstract class BootstringParams extends Product with Serializable {
  def basicCodepoints: BitSet
  def base: Base
  def delimiter: Delimiter
  def tmin: TMin
  def tmax: TMax
  def skew: Skew
  def damp: Damp
  def initialBias: Bias
  def initialN: Int

  override final def toString: String =
    s"BootstringParams(basicCodepoints = ${basicCodepoints}, base = ${base}, delimiter = ${delimiter}, tmin = ${tmin}, tmax = ${tmax}, skew = ${skew}, damp = ${damp}, initialBias = ${initialBias}, initialN = ${initialN})"


}

object BootstringParams {
  private[this] final case class BootstringParamsImpl(
    override val basicCodepoints: BitSet,
    override val base: Base,
    override val delimiter: Delimiter,
    override val tmin: TMin,
    override val tmax: TMax,
    override val skew: Skew,
    override val damp: Damp,
    override val initialBias: Bias,
    override val initialN: Int
  ) extends BootstringParams

  private[this] val punycodeBasicCodePoints: BitSet =
    (0 to 0x7F).toList.foldMap(value => BitSet(value))

  private val PunycodeCodePoints: BitSet =
    (0x41 to 0x51).toList.foldMap((value: Int) => BitSet(value)) ++
    (0x61 to 0x7A).toList.foldMap((value: Int) => BitSet(value)) ++
    (0x30 to 0x39).toList.foldMap((value: Int) => BitSet(value))

  val punycodeParams: BootstringParams =
    unsafeFrom(
      PunycodeCodePoints,
      Base.PunycodeBase,
      Delimiter.PunycodeDelimiter,
      TMin.PunycodeTMin,
      TMax.PunycodeTMax,
      Skew.PunycodeSkew,
      Damp.PunycodeDamp,
      Bias.PunycodeInitialBias,
      128
    )

  def apply(
    basicCodepoints: BitSet,
    base: Base,
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
          basicCodepoints,
          base,
          tmin,
          tmax,
          skew,
          damp,
          initialBias,
          initialN
        )
    }

  def unsafeFrom(
    basicCodepoints: BitSet,
    base: Base,
    tmin: TMin,
    tmax: TMax,
    skew: Skew,
    damp: Damp,
    initialBias: Bias,
    initialN: Int
  ): BootstringParams =
    apply(
      basicCodepoints,
      base,
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
}
