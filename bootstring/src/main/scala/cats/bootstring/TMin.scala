package cats.bootstring

import cats._
import cats.syntax.all._

sealed abstract class TMin extends Product with Serializable {
  def value: Int

  override final def toString: String = s"TMin(value = ${value})"
}

object TMin {
  private[this] final case class TMinImpl(override val value: Int) extends TMin

  val PunycodeTMin: TMin = unsafeFromInt(1)

  def fromInt(value: Int): Either[String, TMin] =
    if (value >= 0) {
      Right(TMinImpl(value))
    } else {
      Left(s"According to RFC-3492 damp values must be >= 0.")
    }

  def unsafeFromInt(value: Int): TMin =
    fromInt(value).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )

  def fromString(value: String): Either[String, TMin] =
    ApplicativeError[Either[Throwable, *], Throwable].catchNonFatal(
      value.toInt
    ).leftMap(_.getLocalizedMessage).flatMap(fromInt)

  def unsafeFromString(value: String): TMin =
    fromString(value).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )
}
