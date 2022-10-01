package org.typelevel.idna4s.build

import cats._

/**
 * Newtype for a Unicode version string.
 */
final private[build] case class UnicodeVersion(value: String) extends AnyVal

object UnicodeVersion {
  val Latest: UnicodeVersion = UnicodeVersion("latest")

  implicit val hashAndOrderForUnicodeVersion: Hash[UnicodeVersion] with Order[UnicodeVersion] =
    new Hash[UnicodeVersion] with Order[UnicodeVersion] {
      override def hash(x: UnicodeVersion): Int = x.hashCode

      override def compare(x: UnicodeVersion, y: UnicodeVersion): Int =
        x.value.compare(y.value)
    }

  implicit def orderingInstance: Ordering[UnicodeVersion] =
    hashAndOrderForUnicodeVersion.toOrdering
}
