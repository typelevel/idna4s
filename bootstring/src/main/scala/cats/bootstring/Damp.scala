package cats.bootstring

import cats._
import cats.syntax.all._

sealed abstract class Damp extends Product with Serializable {
  def value: Int

  override final def toString: String = s"Damp(value = ${value})"
}

object Damp {
  private[this] final case class DampImpl(override val value: Int) extends Damp

  val PunycodeDamp: Damp = unsafeFromInt(700)

  def fromInt(value: Int): Either[String, Damp] =
    if (value >= 2) {
      Right(DampImpl(value))
    } else {
      Left(s"According to RFC-3492 damp values must be >= 2.")
    }

  def unsafeFromInt(value: Int): Damp =
    fromInt(value).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )

  def fromString(value: String): Either[String, Damp] =
    ApplicativeError[Either[Throwable, *], Throwable].catchNonFatal(
      value.toInt
    ).leftMap(_.getLocalizedMessage).flatMap(fromInt)

  def unsafeFromString(value: String): Damp =
    fromString(value).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )
}
