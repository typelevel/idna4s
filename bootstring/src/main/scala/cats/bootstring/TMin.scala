package cats.bootstring

sealed abstract class TMin extends Product with Serializable {
  def value: Long

  override final def toString: String = s"TMin(value = ${value})"
}

object TMin {
  private[this] final case class TMinImpl(override val value: Long) extends TMin

  def apply(value: Long): Either[String, TMin] =
    if (value >= 0) {
      Right(TMinImpl(value))
    } else {
      Left(s"According to RFC-3492 damp values must be >= 0.")
    }

  def unsafeFromLong(value: Long): TMin =
    apply(value).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )
}
