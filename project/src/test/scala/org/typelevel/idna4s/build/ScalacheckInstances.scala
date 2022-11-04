package org.typelevel.idna4s.build

import cats._
import org.scalacheck._
import org.typelevel.idna4s.build.UTS46CodeGen._
import org.typelevel.idna4s.build.UnicodeDataCodeGen._
import scala.collection.immutable.SortedMap

object ScalacheckInstances {
  private def genNonEmptyString(implicit A: Arbitrary[String]): Gen[String] =
    A.arbitrary.suchThat(_.size > 0)

  val genUnicodeNumericVersion: Gen[UnicodeVersion.Numeric] =
    for {
      major <- Gen.choose(0L, Long.MaxValue)
      minor <- Gen.choose(0L, Long.MaxValue)
      patch <- Gen.choose(0L, Long.MaxValue)
    } yield UnicodeVersion.Numeric.unsafeFromLongs(major, minor, patch)

  implicit val arbUnicodeVersion: Arbitrary[UnicodeVersion] =
    Arbitrary(
      Gen.oneOf(
        genUnicodeNumericVersion,
        Gen.const(UnicodeVersion.Latest)
      )
    )

  implicit lazy val cogenUnicodeVersion: Cogen[UnicodeVersion] =
    Cogen[String].contramap(_.asString)

  implicit val chooseCodePoint: Gen.Choose[CodePoint] =
    Gen.Choose.xmap(CodePoint.unsafeFromInt, _.value)

  implicit val arbCodePoint: Arbitrary[CodePoint] =
    Arbitrary(
      Gen.choose(CodePoint.unsafeFromInt(0), CodePoint.unsafeFromInt(Character.MAX_CODE_POINT))
    )

  implicit val cogenCodePoint: Cogen[CodePoint] =
    Cogen[Int].contramap(_.value)

  implicit val arbIDNA2008Status: Arbitrary[IDNA2008Status] =
    Arbitrary(
      Gen.oneOf(IDNA2008Status.NV8, IDNA2008Status.XV8)
    )

  implicit val cogenIDNA2008Status: Cogen[IDNA2008Status] =
    Cogen[String].contramap(_.asString)

  implicit val arbCodePointRange: Arbitrary[CodePointRange] =
    Arbitrary(
      Arbitrary
        .arbitrary[CodePoint]
        .flatMap(lower =>
          Gen
            .choose(lower, CodePoint.unsafeFromInt(Character.MAX_CODE_POINT))
            .map(upper => CodePointRange.unsafeFrom(lower, upper)))
    )

  implicit val cogenCodePointRange: Cogen[CodePointRange] =
    Cogen[(CodePoint, CodePoint)].contramap(value => (value.lower, value.upper))

  implicit val arbRangeType: Arbitrary[RangeType] =
    Arbitrary(
      Gen.oneOf(RangeType.Start, RangeType.End)
    )

  implicit val cogenRangeType: Cogen[RangeType] =
    Cogen[Int].contramap {
      case RangeType.Start => 0
      case RangeType.End => 1
    }

  implicit val arbGeneralCategory: Arbitrary[GeneralCategory] =
    Arbitrary(
      Arbitrary.arbitrary[String].map(GeneralCategory.apply)
    )

  implicit val cogenGeneralCategory: Cogen[GeneralCategory] =
    Cogen[String].contramap(_.value)

  implicit val arbName: Arbitrary[Name] =
    Arbitrary(
      Arbitrary.arbitrary[String].map(Name.apply)
    )

  implicit val cogenName: Cogen[Name] =
    Cogen[String].contramap(_.value)

  implicit val arbCanonicalCombiningClass: Arbitrary[CanonicalCombiningClass] =
    Arbitrary(
      Arbitrary.arbitrary[Int].map(CanonicalCombiningClass.apply)
    )

  implicit val cogenCanonicalCombiningClass: Cogen[CanonicalCombiningClass] =
    Cogen[Int].contramap(_.value)

  implicit val arbBidirectionalCategory: Arbitrary[BidirectionalCategory] =
    Arbitrary(
      Arbitrary.arbitrary[String].map(BidirectionalCategory.apply)
    )

  implicit val cogenBidirectionalCategory: Cogen[BidirectionalCategory] =
    Cogen[String].contramap(_.value)

  implicit val arbCharacterDecompositionMapping: Arbitrary[CharacterDecompositionMapping] =
    Arbitrary(
      genNonEmptyString.map(CharacterDecompositionMapping.unsafeFromString)
    )

  implicit val cogenCharacterDecompositionMapping: Cogen[CharacterDecompositionMapping] =
    Cogen[String].contramap(_.value)

  implicit val arbDecimalDecimalDigitValue: Arbitrary[DecimalDigitValue] =
    Arbitrary(
      Gen.choose(0, 9).map(DecimalDigitValue.unsafeFromInt)
    )

  implicit val cogenDecimalDigitValue: Cogen[DecimalDigitValue] =
    Cogen[Int].contramap(_.value)

  implicit val arbDigitValue: Arbitrary[DigitValue] =
    Arbitrary(
      Gen.choose(0, Int.MaxValue).map(DigitValue.unsafeFromInt)
    )

  implicit val cogenDigitValue: Cogen[DigitValue] =
    Cogen[Int].contramap(_.value)

  implicit val arbNumericValue: Arbitrary[NumericValue] =
    Arbitrary(
      genNonEmptyString.map(NumericValue.unsafeFromString)
    )

  implicit val cogenNumericValue: Cogen[NumericValue] =
    Cogen[String].contramap(_.value)

  implicit val arbMirroredValue: Arbitrary[Mirrored] =
    Arbitrary(
      Arbitrary.arbitrary[Boolean].map(Mirrored.apply)
    )

  implicit val cogenMirroedValue: Cogen[Mirrored] =
    Cogen[Boolean].contramap(_.value)

  implicit val arbUnicode1Name: Arbitrary[Unicode1Name] =
    Arbitrary(
      genNonEmptyString.map(Unicode1Name.unsafeFromString)
    )

  implicit val cogenUnicode1Name: Cogen[Unicode1Name] =
    Cogen[String].contramap(_.value)

  implicit val arbISO10646Comment: Arbitrary[ISO10646Comment] =
    Arbitrary(
      genNonEmptyString.map(ISO10646Comment.unsafeFromString)
    )

  implicit val cogenISO10646Comment: Cogen[ISO10646Comment] =
    Cogen[String].contramap(_.value)

  implicit val arbUppercaseMapping: Arbitrary[UppercaseMapping] =
    Arbitrary(
      genNonEmptyString.map(UppercaseMapping.unsafeFromString)
    )

  implicit val cogenUppercaseMapping: Cogen[UppercaseMapping] =
    Cogen[String].contramap(_.value)

  implicit val arbLowercaseMapping: Arbitrary[LowercaseMapping] =
    Arbitrary(
      genNonEmptyString.map(LowercaseMapping.unsafeFromString)
    )

  implicit val cogenLowercaseMapping: Cogen[LowercaseMapping] =
    Cogen[String].contramap(_.value)

  implicit val arbTitlecaseMapping: Arbitrary[TitlecaseMapping] =
    Arbitrary(
      genNonEmptyString.map(TitlecaseMapping.unsafeFromString)
    )

  implicit val cogenTitlecaseMapping: Cogen[TitlecaseMapping] =
    Cogen[String].contramap(_.value)

  implicit val arbUnicodeCodePointInformation: Arbitrary[UnicodeCodePointInfomation] =
    Arbitrary(
      for {
        name <- Arbitrary.arbitrary[Name]
        generalCategory <- Arbitrary.arbitrary[GeneralCategory]
        canonicalCombiningClass <- Arbitrary.arbitrary[CanonicalCombiningClass]
        bidirectionalCategory <- Arbitrary.arbitrary[BidirectionalCategory]
        characterDecompositionMapping <- Arbitrary
          .arbitrary[Option[CharacterDecompositionMapping]]
        decimalDigitValue <- Arbitrary.arbitrary[Option[DecimalDigitValue]]
        digitValue <- Arbitrary.arbitrary[Option[DigitValue]]
        numericValue <- Arbitrary.arbitrary[Option[NumericValue]]
        mirrored <- Arbitrary.arbitrary[Mirrored]
        unicode1Name <- Arbitrary.arbitrary[Option[Unicode1Name]]
        iso10646Comment <- Arbitrary.arbitrary[Option[ISO10646Comment]]
        uppercaseMapping <- Arbitrary.arbitrary[Option[UppercaseMapping]]
        lowercaseMapping <- Arbitrary.arbitrary[Option[LowercaseMapping]]
        titlecaseMapping <- Arbitrary.arbitrary[Option[TitlecaseMapping]]
      } yield UnicodeCodePointInfomation(
        name,
        generalCategory,
        canonicalCombiningClass,
        bidirectionalCategory,
        characterDecompositionMapping,
        decimalDigitValue,
        digitValue,
        numericValue,
        mirrored,
        unicode1Name,
        iso10646Comment,
        uppercaseMapping,
        lowercaseMapping,
        titlecaseMapping
      ))

  implicit val cogenUnicodeCodePointInfomation: Cogen[UnicodeCodePointInfomation] =
    Cogen[(
        Name,
        GeneralCategory,
        CanonicalCombiningClass,
        BidirectionalCategory,
        Option[CharacterDecompositionMapping],
        Option[DecimalDigitValue],
        Option[DigitValue],
        Option[NumericValue],
        Mirrored,
        Option[Unicode1Name],
        Option[ISO10646Comment],
        Option[UppercaseMapping],
        Option[LowercaseMapping],
        Option[TitlecaseMapping])].contramap(value =>
      (
        value.name,
        value.generalCategory,
        value.canonicalCombiningClass,
        value.bidirectionalCategory,
        value.characterDecompositionMapping,
        value.decimalDigitValue,
        value.digitValue,
        value.numericValue,
        value.mirrored,
        value.unicode1Name,
        value.iso10646Comment,
        value.uppercaseMapping,
        value.lowercaseMapping,
        value.titlecaseMapping))

  implicit val arbUnicodeDataRow: Arbitrary[UnicodeDataRow] =
    Arbitrary(
      for {
        codeValue <- Arbitrary.arbitrary[CodePoint]
        rangeType <- Arbitrary.arbitrary[Option[RangeType]]
        unicodeCodePointInformation <- Arbitrary.arbitrary[UnicodeCodePointInfomation]
      } yield UnicodeDataRow(codeValue, rangeType, unicodeCodePointInformation)
    )

  implicit val cogenUnicodeDataRow: Cogen[UnicodeDataRow] =
    Cogen[(CodePoint, Option[RangeType], UnicodeCodePointInfomation)].contramap(value =>
      (value.codeValue, value.rangeType, value.unicodeCodePointInformation))

  implicit def arbUnicodeData[A: Arbitrary: Order]: Arbitrary[UnicodeData[A]] =
    Arbitrary(
      Arbitrary.arbitrary[SortedMap[CodePointRange, A]].map(value => UnicodeData(value))
    )

  implicit def cogenUnicodeData[A: Cogen]: Cogen[UnicodeData[A]] =
    Cogen[Map[CodePointRange, A]].contramap(
      _.asSortedMap.toMap
    )
}
