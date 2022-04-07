package cats.bootstring

sealed abstract class Base extends Product with Serializable {
  def value: Int

  final def decodeDigit(codePoint: Int): Either[String, Int] =
    Base.decodeDigit(this)(codePoint)

  override final def toString: String = s"Base(value = ${value})"
}

object Base {
  private[this] final case class BaseImpl(override val value: Int) extends Base

  private[this] def codePointToIntegralValue(value: Int): Either[String, Int] =
    if (value >= 48 && value <= 57) {
      Right(value - 48)
    } else if (value >= 65 && value <= 90) {
      Right(value - 55)
    } else if (value >= 97 && value <= 122) {
      Right(value - 87)
    } else {
      Left(s"Code point value does not corrispond to any know base encoding: $value")
    }

  val PunycodeBase: Base =
    Base.unsafeFromInt(36)

  def decodeDigit(base: Base)(codePoint: Int): Either[String, Int] =
    codePointToIntegralValue(codePoint).flatMap{
      case value if value >= base.value =>
        Left(s"Value exceeds digit in the given base, value: ${value}, base: ${base}")
      case value =>
        Right(value)
    }

  final def unsafeDecodeDigit(base: Base)(codePoint: Int): Int =
    decodeDigit(base)(codePoint).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )

  def fromInt(value: Int): Either[String, Base] =
    if (value > 0 && value < 37) {
      Right(BaseImpl(value))
    } else {
      if (value <= 0) {
        Left(s"According to RFC-3492 base values must be > 0.")
      } else {
        Left(s"Base values > 36 are not supported as we don't have an obvious set of characters to used for digits > than Z.")
      }
    }

  def unsafeFromInt(value: Int): Base =
    fromInt(value).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )
}
