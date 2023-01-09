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

import cats.kernel.laws.discipline._
import munit._
import org.typelevel.idna4s.build.ScalacheckInstances._
import org.typelevel.idna4s.build.UTS46CodeGen._
import org.typelevel.idna4s.build.UnicodeDataCodeGen._

final class LawChecks extends DisciplineSuite {
  checkAll("Order[UnicodeVersion]", OrderTests[UnicodeVersion].order)
  checkAll("Hash[UnicodeVersion]", HashTests[UnicodeVersion].hash)

  checkAll("Order[CodePoint]", OrderTests[CodePoint].order)

  checkAll("Order[IDNA2008Status]", OrderTests[IDNA2008Status].order)

  checkAll("Order[CodePointRange]", OrderTests[CodePointRange].order)
  checkAll("Hash[CodePointRange]", HashTests[CodePointRange].hash)

  checkAll("Order[RangeType]", OrderTests[RangeType].order)
  checkAll("Hash[RangeType]", HashTests[RangeType].hash)

  checkAll("Order[GeneralCategory]", OrderTests[GeneralCategory].order)
  checkAll("Hash[GeneralCategory]", HashTests[GeneralCategory].hash)

  checkAll("Order[Name]", OrderTests[Name].order)
  checkAll("Hash[Name]", HashTests[Name].hash)

  checkAll("Order[CanonicalCombiningClass]", OrderTests[CanonicalCombiningClass].order)
  checkAll("Hash[CanonicalCombiningClass]", HashTests[CanonicalCombiningClass].hash)

  checkAll("Order[BidirectionalCategory]", OrderTests[BidirectionalCategory].order)
  checkAll("Hash[BidirectionalCategory]", HashTests[BidirectionalCategory].hash)

  checkAll(
    "Order[CharacterDecompositionMapping]",
    OrderTests[CharacterDecompositionMapping].order)
  checkAll("Hash[CharacterDecompositionMapping]", HashTests[CharacterDecompositionMapping].hash)

  checkAll("Order[DecimalDigitValue]", OrderTests[DecimalDigitValue].order)
  checkAll("Hash[DecimalDigitValue]", HashTests[DecimalDigitValue].hash)

  checkAll("Order[DigitValue]", OrderTests[DigitValue].order)
  checkAll("Hash[DigitValue]", HashTests[DigitValue].hash)

  checkAll("Order[NumericValue]", OrderTests[NumericValue].order)
  checkAll("Hash[NumericValue]", HashTests[NumericValue].hash)

  checkAll("Order[Mirrored]", OrderTests[Mirrored].order)
  checkAll("Hash[Mirrored]", HashTests[Mirrored].hash)

  checkAll("Order[Unicode1Name]", OrderTests[Unicode1Name].order)
  checkAll("Hash[Unicode1Name]", HashTests[Unicode1Name].hash)

  checkAll("Order[ISO10646Comment]", OrderTests[ISO10646Comment].order)
  checkAll("Hash[ISO10646Comment]", HashTests[ISO10646Comment].hash)

  checkAll("Order[UppercaseMapping]", OrderTests[UppercaseMapping].order)
  checkAll("Hash[UppercaseMapping]", HashTests[UppercaseMapping].hash)

  checkAll("Order[LowercaseMapping]", OrderTests[LowercaseMapping].order)
  checkAll("Hash[LowercaseMapping]", HashTests[LowercaseMapping].hash)

  checkAll("Order[TitlecaseMapping]", OrderTests[TitlecaseMapping].order)
  checkAll("Hash[TitlecaseMapping]", HashTests[TitlecaseMapping].hash)

  checkAll("Order[UnicodeCodePointInfomation]", OrderTests[UnicodeCodePointInfomation].order)
  checkAll("Hash[UnicodeCodePointInfomation]", HashTests[UnicodeCodePointInfomation].hash)

  checkAll("Order[UnicodeDataRow]", OrderTests[UnicodeDataRow].order)
  checkAll("Hash[UnicodeDataRow]", HashTests[UnicodeDataRow].hash)

  checkAll("Order[UnicodeData[Int]]", OrderTests[UnicodeData[Int]].order)
  checkAll("Hash[UnicodeData[Int]]", HashTests[UnicodeData[Int]].hash)

  checkAll(
    "Order[DerivedJoiningTypeCodeGen.JoiningType]",
    OrderTests[DerivedJoiningTypeCodeGen.JoiningType].order)
  checkAll(
    "Hash[DerivedJoiningTypeCodeGen.JoiningType]",
    HashTests[DerivedJoiningTypeCodeGen.JoiningType].hash)

  checkAll(
    "Order[DerivedJoiningTypeCodeGen.Row]",
    OrderTests[DerivedJoiningTypeCodeGen.Row].order)
  checkAll("Hash[DerivedJoiningTypeCodeGen.Row]", HashTests[DerivedJoiningTypeCodeGen.Row].hash)
}
