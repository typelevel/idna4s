package cats.bootstring

sealed abstract class Delimiter extends Product with Serializable {
  def codePoint: Int

  final def charString: String =
    new String(Character.toChars(value))

  override final def toString: String = s"Delimiter(codePoint = ${value}, charString = ${charString})"
}

object Delimiter {
  private[this] final case class DelimiterImpl(override val codePoint: Int) extends Delimiter

  val PunycodeDelimiter: Delimiter = fromChar('-')

  def fromInt(codePoint: Int): Either[String, Delimiter] =
    if (value >= 0 && value < 0x10ffff) {
      Right(DelimiterImp(codePoint))
    } else {
      Left(s"Not a valid Unicode code point: ${codePoint}")
    }

  def fromChar(char: Char): Either[String, Delimiter] =
    char.toInt match {
      case value if value < Character.MIN_SURROGATE =>
        fromInt(value)
      case _ =>
        Left(s"Char is part of a surrogate pair, but that is not a valid code point in isolation: '${char}'")
    }

  def fromSurrogatePair(high: Char, low: Char): Either[NonEmptyList[String], Delimiter] =


  def unsafeFromCodePoint(value: Int): Delimiter =
    fromInt(value).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )
}
