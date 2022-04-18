package cats.bootstring

sealed abstract class Base extends Product with Serializable {
  def value: Int

  private val lowercaseCharCodePoints: Array[Int] =
    Base.lowercaseCharList.take(value).map(_.toInt).toArray

  private val uppercaseCharCodePoints: Array[Int] =
    Base.uppercaseCharList.take(value).map(_.toInt).toArray

  final def decodeDigit(codePoint: Int): Either[String, Int] =
    Base.codePointToIntegralValue(codePoint).flatMap{
      case digit if digit >= value =>
        Left(s"Value exceeds digit in the given base, value: ${digit}, base: ${this}")
      case digit =>
        Right(digit)
    }

  final def unsafeDecodeDigit(codePoint: Int): Int =
    decodeDigit(codePoint).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )

  final def encodeToDigitCodePoint(digit: Int, uppercase: Boolean = false): Either[String, Int] =
    if (digit < value && digit >= 0) {
      Right(
        if (uppercase) {
          uppercaseCharCodePoints(digit)
        } else {
          lowercaseCharCodePoints(digit)
        }
      )
    } else {
      Left(s"Integral value ${digit}, is not in the domain of base $this")
    }

  final def unsafeEncodeToDigitCodePoint(digit: Int, uppercase: Boolean = false): Int =
    encodeToDigitCodePoint(digit, uppercase).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )

  override final def toString: String = s"Base(value = ${value})"
}

object Base {
  private[this] final case class BaseImpl(override val value: Int) extends Base

  private def codePointToIntegralValue(value: Int): Either[String, Int] =
    if (value >= 48 && value <= 57) {
      Right(value - 48)
    } else if (value >= 65 && value <= 90) {
      Right(value - 55)
    } else if (value >= 97 && value <= 122) {
      Right(value - 97)
    } else {
      Left(s"Code point value does not corrispond to any know base encoding: $value")
    }

  private val lowercaseCharList: List[Char] =
    (Range.inclusive('0', '9') ++ Range.inclusive('a', 'z')).toList.map(_.toChar)

  private val uppercaseCharList: List[Char] =
    (Range.inclusive('0', '9') ++ Range.inclusive('A', 'Z')).toList.map(_.toChar)

  val PunycodeBase: Base =
    Base.unsafeFromInt(36)

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
