package org.typelevel.idna4s.build

import cats._
import cats.syntax.all._

/**
 * Newtype for a Unicode code point.
 */
final private[build] class CodePoint private (val value: Int) extends AnyVal {
  final override def toString: String = s"CodePoint(value = ${value})"
}

private[build] object CodePoint {
  private def apply(value: Int): CodePoint =
    new CodePoint(value)

  def unsafeFromInt(value: Int): CodePoint =
    fromInt(value).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )

  def fromInt(value: Int): Either[String, CodePoint] =
    if (value < 0 || value > Character.MAX_CODE_POINT) {
      Left(s"Invalid code point: ${value}")
    } else {
      Right(apply(value))
    }

  def fromHexString(value: String): Either[String, CodePoint] = {
    val trimmed: String = value.trim.toLowerCase
    val integralValue: Either[Throwable, Int] =
      if (trimmed.startsWith("0x")) {
        Either.catchNonFatal(Integer.parseInt(trimmed.drop(2), 16))
      } else {
        Either.catchNonFatal(Integer.parseInt(trimmed, 16))
      }
    integralValue.leftMap(_.getLocalizedMessage).flatMap(fromInt)
  }

  def unsafeFromHexString(value: String): CodePoint =
    fromHexString(value).fold(e => throw new IllegalArgumentException(e), identity)

  implicit val orderInstance: Order[CodePoint] =
    Order.by(_.value)

  implicit def orderingInstance: Ordering[CodePoint] =
    orderInstance.toOrdering
}
