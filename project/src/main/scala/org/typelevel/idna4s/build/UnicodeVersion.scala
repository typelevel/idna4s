/*
 * Copyright 2023 Typelevel
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.typelevel.idna4s.build

import cats._
import scala.util.matching.Regex

sealed abstract private[build] class UnicodeVersion extends Serializable {
  def asString: String
}

private[build] object UnicodeVersion {

  def fromString(value: String): Either[String, UnicodeVersion] =
    value.trim.toLowerCase match {
      case "latest" =>
        Right(Latest)
      case otherwise =>
        Numeric.fromString(otherwise)
    }

  def unsafeFromString(value: String): UnicodeVersion =
    fromString(value).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )

  sealed abstract class Numeric extends UnicodeVersion {
    def major: String
    def minor: String
    def patch: String

    final override def asString: String = s"${major}.${minor}.${patch}"

    final override def toString: String =
      s"UnicodeVersion.Numeric(major = ${major}, minor = ${minor}, patch = ${patch})"
  }

  object Numeric {
    final private[this] case class NumericImpl(
        override val major: String,
        override val minor: String,
        override val patch: String)
        extends Numeric

    private val versionRegex: Regex =
      """\s*([1-9]\d*|0)\.([1-9]\d*|0)\.([1-9]\d*|0)\s*""".r

    def fromString(value: String): Either[String, Numeric] =
      value match {
        case versionRegex(major, minor, patch) =>
          Right(NumericImpl(major, minor, patch))
        case _ =>
          Left(s"Given string is not a valid Unicode version: ${value}")
      }

    def unsafeFromString(value: String): Numeric =
      fromString(value).fold(
        e => throw new IllegalArgumentException(e),
        identity
      )

    def fromLongs(major: Long, minor: Long, patch: Long): Either[String, Numeric] =
      if (major >= 0L && minor >= 0L && patch >= 0L) {
        Right(NumericImpl(major.toString, minor.toString, patch.toString))
      } else {
        Left(s"A numeric Unicode version value can not have negative components.")
      }

    def unsafeFromLongs(major: Long, minor: Long, patch: Long): Numeric =
      fromLongs(major, minor, patch).fold(
        s => throw new IllegalArgumentException(s),
        identity
      )
  }

  case object Latest extends UnicodeVersion {
    final override def asString: String = "latest"

    override val toString: String = "UnicodeVersion.Latest"
  }

  implicit val hashAndOrderForUnicodeVersion: Hash[UnicodeVersion] with Order[UnicodeVersion] =
    new Hash[UnicodeVersion] with Order[UnicodeVersion] {
      override def hash(x: UnicodeVersion): Int = x.hashCode

      override def compare(x: UnicodeVersion, y: UnicodeVersion): Int =
        (x, y) match {
          case (Latest, Latest) =>
            0
          case (Latest, _) =>
            1
          case (_, Latest) =>
            -1
          case (x: Numeric, y: Numeric) =>
            x.major.compare(y.major) match {
              case 0 =>
                x.minor.compare(y.minor) match {
                  case 0 =>
                    x.patch.compare(y.patch)
                  case otherwise =>
                    otherwise
                }
              case otherwise =>
                otherwise
            }
        }
    }

  implicit def orderingInstance: Ordering[UnicodeVersion] =
    hashAndOrderForUnicodeVersion.toOrdering
}
