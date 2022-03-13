package cats.bootstring

sealed abstract class Skew extends Product with Serializable {
  def value: Long

  override final def toString: String = s"Skew(value = ${value})"
}

object Skew {
  private[this] final case class SkewImpl(override val value: Long) extends Skew

  def apply(value: Long): Either[String, Skew] =
    if (value >= 1) {
      Right(SkewImpl(value))
    } else {
      Left(s"According to RFC-3492 skew values must be >= 1.")
    }

  def unsafeFromLong(value: Long): Skew =
    apply(value).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )
}
