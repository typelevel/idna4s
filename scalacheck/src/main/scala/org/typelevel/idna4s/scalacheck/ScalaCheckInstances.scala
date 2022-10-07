/*
 * Copyright (c) 2022 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package org.typelevel.idna4s.scalacheck

import cats.data._
import cats.syntax.all._
import org.scalacheck.Gen.Choose
import org.scalacheck._
import org.typelevel.idna4s.core._
import org.typelevel.idna4s.core.uts46.CodePointMapper._
import org.typelevel.idna4s.core.uts46._
import org.typelevel.idna4s.core.bootstring._
import scala.annotation.nowarn

@nowarn("msg=deprecated")
private[scalacheck] trait ScalaCheckInstances extends Serializable {

  implicit final def chooseCodePoint: Choose[CodePoint] =
    Choose.xmap[Int, CodePoint](CodePoint.unsafeFromInt, _.value)

  implicit final def arbCodePoint: Arbitrary[CodePoint] =
    Arbitrary(Gen.choose(CodePoint.MinValue, CodePoint.MaxValue))

  implicit final def cogenCodePoint: Cogen[CodePoint] =
    Cogen[Int].contramap(_.value)

  implicit final def shrinkCodePoint: Shrink[CodePoint] = {

    // Based on ShrinkIntegral from ScalaCheck, but without negation.
    // https://github.com/typelevel/scalacheck/blob/v1.16.0/core/shared/src/main/scala/org/scalacheck/Shrink.scala#L308
    def halves(x: CodePoint): Stream[CodePoint] = {
      val q = CodePoint.unsafeFromInt(x.value / 2)
      if (q <= CodePoint.MinValue) {
        Stream(CodePoint.MinValue)
      } else {
        q #:: halves(q)
      }
    }

    Shrink(cp =>
      if (cp.value === 0) {
        Stream.empty[CodePoint]
      } else {
        halves(cp)
      })
  }

  implicit final def arbCodePointStatus: Arbitrary[CodePointStatus] = {
    import CodePointStatus._
    val genNonEmptyListOfCodePoint: Gen[NonEmptyList[CodePoint]] =
      Gen.nonEmptyListOf(Arbitrary.arbitrary[CodePoint]).map(NonEmptyList.fromListUnsafe)

    Arbitrary(
      Gen.oneOf(
        Gen.const(Valid.always),
        Gen.const(Valid.nv8),
        Gen.const(Valid.xv8),
        Gen.const(Ignored.instance),
        genNonEmptyListOfCodePoint.map(Mapped.of),
        Gen.oneOf(
          Gen.const(Deviation.ignored),
          genNonEmptyListOfCodePoint.map(Deviation.of)
        ),
        Gen.const(Disallowed.instance),
        Gen.const(Disallowed_STD3_Valid.instance),
        genNonEmptyListOfCodePoint.map(Disallowed_STD3_Mapped.of)
      )
    )
  }

  implicit final def cogenCodePointStatus: Cogen[CodePointStatus] =
    Cogen[String].contramap(_.toString) // Contrived, but correct

  implicit final def arbCodePointMappingException: Arbitrary[CodePointMappingException] =
    Arbitrary(
      Arbitrary.arbitrary[MappingException].flatMap(value => Gen.oneOf(value.errors.toList))
    )

  implicit final def cogenCodePointMappingException: Cogen[CodePointMappingException] =
    Cogen[(Int, String, CodePoint)].contramap(value =>
      (value.failureIndex, value.message, value.codePoint))

  /**
   * A generator which will generate an input String to `CodePointMapper#mapCodePoints` which
   * will fail and the `MappingException` yield by said failure.
   */
  final def genFailingStringAndMappingException: Gen[(String, MappingException)] =
    Arbitrary.arbitrary[(Boolean, Boolean)].flatMap {
      case (useStd3ASCIIRules, transitionalProcessing) =>
        Arbitrary
          .arbitrary[String]
          .flatMap(value =>
            CodePointMapper
              .mapCodePoints(useStd3ASCIIRules, transitionalProcessing)(value)
              .fold(
                e => Gen.const(value -> e),
                _ => Gen.fail[(String, MappingException)]
              ))
    }

  implicit final def arbMappingException: Arbitrary[MappingException] =
    Arbitrary(
      genFailingStringAndMappingException.map(_._2)
    )

  implicit final def cogenMappingException: Cogen[MappingException] =
    Cogen[(List[CodePointMappingException], String)].contramap(value =>
      (value.errors.toList, value.partiallyMappedInput))

  implicit final def arbIDNA2008Status: Arbitrary[IDNA2008Status] =
    Arbitrary(Gen.oneOf(IDNA2008Status.NV8, IDNA2008Status.XV8))

  implicit final def cogenIDna2008Status: Cogen[IDNA2008Status] =
    Cogen[Int].contramap {
      case IDNA2008Status.NV8 =>
        0
      case IDNA2008Status.XV8 =>
        1
    }

  /**
   * Generator for int values which are not Unicode code points.
   */
  final def genNonCodePoint: Gen[Int] =
    Gen.oneOf(
      Gen.choose(Int.MinValue, -1),
      Gen.choose(Character.MAX_CODE_POINT + 1, Int.MaxValue)
    )

  final def genInvalidSurrogatePair: Gen[(Char, Char)] = {
    val genNotSurrogate: Gen[Char] =
      Gen.choose(Char.MinValue, (Character.MIN_SURROGATE.toInt - 1).toChar)
    val genNotLowSurrogate: Gen[Char] =
      Gen.oneOf(
        genNotSurrogate,
        Gen.choose(Character.MIN_HIGH_SURROGATE, Character.MAX_HIGH_SURROGATE)
      )
    val genNotHighSurrogate: Gen[Char] =
      Gen.oneOf(
        genNotSurrogate,
        Gen.choose(Character.MIN_LOW_SURROGATE, Character.MAX_LOW_SURROGATE)
      )

    genNotLowSurrogate.flatMap(notLow => genNotHighSurrogate.map(notHigh => notLow -> notHigh))
  }

  implicit final def chooseDamp: Choose[Damp] =
    Choose.xmap[Int, Damp](Damp.unsafeFromInt, _.value)

  implicit final def arbDamp: Arbitrary[Damp] =
    Arbitrary(Gen.choose(Damp.MinValue, Damp.MaxValue))

  implicit final def cogenDamp: Cogen[Damp] =
    Cogen[Int].contramap(_.value)

  implicit final def chooseDelimiter: Choose[Delimiter] =
    Choose.xmap[Int, Delimiter](Delimiter.unsafeFromInt, _.codePointInt)

  implicit final def arbDelimiter: Arbitrary[Delimiter] =
    Arbitrary(
      Gen
        .oneOf(
          Gen.choose(0, Character.MIN_SURROGATE - 1),
          Gen.choose(Character.MAX_SURROGATE + 1, Character.MAX_CODE_POINT)
        )
        .map(Delimiter.unsafeFromInt)
    )

  implicit final def cogenDelimiter: Cogen[Delimiter] =
    Cogen[Int].contramap(_.codePointInt)

  implicit final def shrinkDelimiter: Shrink[Delimiter] =
    Shrink(value =>
      shrinkCodePoint
        .shrink(value.codePoint)
        .filterNot(_.isSurrogate)
        .map(Delimiter.unsafeFromCodePoint))

  implicit final def arbUTS46Config: Arbitrary[UTS46Config] =
    Arbitrary(
      for {
        checkHyphens <- Arbitrary.arbitrary[Boolean]
        checkBidi <- Arbitrary.arbitrary[Boolean]
        checkJoiners <- Arbitrary.arbitrary[Boolean]
        useStd3ASCIIRules <- Arbitrary.arbitrary[Boolean]
        transitionalProcessing <- Arbitrary.arbitrary[Boolean]
        verifyDnsLength <- Arbitrary.arbitrary[Boolean]
      } yield UTS46Config(checkHyphens, checkBidi, checkJoiners, useStd3ASCIIRules, transitionalProcessing, verifyDnsLength)
    )

  implicit final def cogenUTS46Config: Cogen[UTS46Config] =
    Cogen[(Boolean, Boolean, Boolean, Boolean, Boolean, Boolean)].contramap(value =>
      (value.checkHyphens, value.checkBidi, value.checkJoiners, value.useStd3ASCIIRules, value.transitionalProcessing, value.verifyDnsLength)
    )
}
