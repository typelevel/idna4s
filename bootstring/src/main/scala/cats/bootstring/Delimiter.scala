package cats.bootstring

import cats.data._
import cats.syntax.all._

sealed abstract class Delimiter extends Product with Serializable {
  def codePoint: Int

  final def charString: String =
    new String(Character.toChars(codePoint))

  override final def toString: String = s"Delimiter(codePoint = ${codePoint}, charString = ${charString})"
}

object Delimiter {
  private[this] final case class DelimiterImpl(override val codePoint: Int) extends Delimiter

  val PunycodeDelimiter: Delimiter = unsafeFromChar('-')

  def fromCodePoint(codePoint: Int): Either[String, Delimiter] =
    if (codePoint >= 0 && codePoint < 0x10ffff) {
      Right(DelimiterImpl(codePoint))
    } else {
      Left(s"Not a valid Unicode code point: ${codePoint}")
    }

  def fromChar(char: Char): Either[String, Delimiter] =
    char.toInt match {
      case value if value < Character.MIN_SURROGATE =>
        fromCodePoint(value)
      case _ =>
        Left(s"Char is part of a surrogate pair, but that is not a valid code point in isolation: '${char}'")
    }

  def fromSurrogatePair(high: Char, low: Char): Either[NonEmptyList[String], Delimiter] =
    (s"Character $high is not a high surrogate unicode character.".leftNel[Delimiter].unlessA(Character.isHighSurrogate(high)),
      s"Character $low is not a low surrogate unicode character.".leftNel[Delimiter].unlessA(Character.isLowSurrogate(low))
    ).parTupled.flatMap{
      case _ => fromCodePoint(Character.toCodePoint(high, low)).leftMap(NonEmptyList.one)
    }

  def unsafeFromCodePoint(value: Int): Delimiter =
    fromCodePoint(value).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )

  def unsafeFromChar(char: Char): Delimiter =
    fromChar(char).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )

  def unsafeFromSurrogatePair(high: Char, low: Char): Delimiter =
    fromSurrogatePair(high, low).fold(
      es => throw new IllegalArgumentException(es.mkString_(", ")),
      identity
    )

  def fromString(value: String): Either[NonEmptyList[String], Delimiter] =
    if (value.length < 3) {
      value.toList match {
        case high :: low :: Nil =>
          fromSurrogatePair(high, low)
        case char :: Nil =>
          fromChar(char).leftMap(NonEmptyList.one)
        case Nil =>
          "The empty string is not a valid boostring delimiter.".leftNel
        case _ =>
          // Not possible
          s"A bootstring delimiter must be a single code point, the given value is invalid: ${value}".leftNel
      }
    } else {
      s"A bootstring delimiter must be a single code point, the given value is invalid: ${value}".leftNel
    }

  def unsafeFromString(value: String): Delimiter =
    fromString(value).fold(
      es => throw new IllegalArgumentException(es.mkString_(", ")),
      identity
    )
}
