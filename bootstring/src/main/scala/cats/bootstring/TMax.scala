package cats.bootstring

sealed abstract class TMax extends Product with Serializable {
  def value: Long

  override final def toString: String = s"TMax(value = ${value})"
}

object TMax {
  private[this] final case class TMaxImpl(override val value: Long) extends TMax

  def apply(value: Long): Either[String, TMax] =
    if (value >= 0) {
      Right(TMaxImpl(value))
    } else {
      Left(s"According to RFC-3492 damp values must be >= 0.")
    }

  def unsafeFromLong(value: Long): TMax =
    apply(value).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )
}
