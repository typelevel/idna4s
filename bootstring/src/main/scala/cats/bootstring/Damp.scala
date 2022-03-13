package cats.bootstring

sealed abstract class Damp extends Product with Serializable {
  def value: Long

  override final def toString: String = s"Damp(value = ${value})"
}

object Damp {
  private[this] final case class DampImpl(override val value: Long) extends Damp

  def apply(value: Long): Either[String, Damp] =
    if (value >= 2) {
      Right(DampImpl(value))
    } else {
      Left(s"According to RFC-3492 damp values must be >= 2.")
    }

  def unsafeFromLong(value: Long): Damp =
    apply(value).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )
}
