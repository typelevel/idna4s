package cats.bootstring

import cats.syntax.all._
import scala.collection.immutable.BitSet

sealed abstract class BootstringParams extends Product with Serializable {
  def basicCodepointPred: Int => Boolean

  override def toString: String =
    s"BootstringParams(basicCodepointPred = Int => Pred)"
}

object BootstringParams {
  private[this] final case class BootstringParamsImpl(
    override val basicCodepointPred: Int => Boolean
  ) extends BootstringParams

  private[this] val punycodeBasicCodePoints: BitSet =
    (0 to 0x7F).toList.foldMap(value => BitSet(value))

  val punycodeParams: BootstringParams =
    unsafeFrom(
      basicCodepointPred = punycodeBasicCodePoints.contains
    )

  def apply(
    basicCodepointPred: Int => Boolean
  ): Either[String, BootstringParams] =
    Right(BootstringParamsImpl(basicCodepointPred))

  def unsafeFrom(basicCodepointPred: Int => Boolean): BootstringParams =
    apply(basicCodepointPred).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )
}
