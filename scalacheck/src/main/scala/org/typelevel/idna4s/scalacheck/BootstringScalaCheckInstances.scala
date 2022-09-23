package org.typelevel.idna4s.scalacheck

import org.scalacheck._
import org.scalacheck.Gen.Choose
import org.typelevel.idna4s.core.bootstring._

private[scalacheck] trait BootstringScalaCheckInstances extends Serializable {
  implicit final def chooseDamp: Choose[Damp] =
    Choose.xmap[Int, Damp](Damp.unsafeFromInt, _.value)

  implicit final def arbDamp: Arbitrary[Damp] =
    Arbitrary(Gen.choose(Damp.MinValue, Damp.MaxValue))

  implicit final def cogenDamp: Cogen[Damp] =
    Cogen[Int].contramap(_.value)
}
