package cats.bootstring

import cats._
import cats.syntax.all._

sealed abstract class Skew extends Product with Serializable {
  def value: Int

  override final def toString: String = s"Skew(value = ${value})"
}

object Skew {
  private[this] final case class SkewImpl(override val value: Int) extends Skew

  val PunycodeSkew: Skew = unsafeFromInt(38)

  def fromInt(value: Int): Either[String, Skew] =
    if (value >= 1) {
      Right(SkewImpl(value))
    } else {
      Left(s"According to RFC-3492 skew values must be >= 1.")
    }

  def unsafeFromInt(value: Int): Skew =
    fromInt(value).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )

  def fromString(value: String): Either[String, Skew] =
    ApplicativeError[Either[Throwable, *], Throwable].catchNonFatal(
      value.toInt
    ).leftMap(_.getLocalizedMessage).flatMap(fromInt)

  def unsafeFromString(value: String): Skew =
    fromString(value).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )
}
