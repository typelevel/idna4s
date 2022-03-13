package cats.bootstring

sealed abstract class Base extends Product with Serializable {
  def value: Long

  override final def toString: String = s"Base(value = ${value})"
}

object Base {
  private[this] final case class BaseImpl(override val value: Long) extends Base

  def apply(value: Long): Either[String, Base] =
    if (value >= 0) {
      Right(BaseImpl(value))
    } else {
      Left(s"According to RFC-3492 damp values must be >= 0.")
    }

  def unsafeFromLong(value: Long): Base =
    apply(value).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )
}
