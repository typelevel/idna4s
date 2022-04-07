package cats.bootstring

import scala.annotation.tailrec

sealed abstract class Bias extends Product with Serializable {
  def value: Int

  override final def toString: String = s"Bias(value = ${value})"
}

object Bias {
  private[this] final case class BiasImpl(override val value: Int) extends Bias

  val PunycodeInitialBias: Bias = unsafeInitialFromInt(72)

  def adapt(bias: Bias, damp: Damp, base: Base, tmin: TMin, tmax: TMax, skew: Skew)(delta: Int, numpoints: Int, firstTime: Boolean): Either[String, Bias] = {

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
          delta / numpoints
        } else {
          delta / 2
        }

      val delta1: Int =
        delta0 + (delta0 / numpoints)

      val (delta2, k) = loop(delta1, 0)

      Right(BiasImpl(k + (((base.value - tmin.value + 1) * delta2) / (delta2 + skew.value))))
    } else {
      Left(s"The number of encoded/decoded codepoints must be > 0 when adapting the bias, but was ${numpoints}")
    }
  }

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
}
