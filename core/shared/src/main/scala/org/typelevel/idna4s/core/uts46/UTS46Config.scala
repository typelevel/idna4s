package org.typelevel.idna4s.core.uts46

sealed abstract class UTS46Config extends Serializable {
  def checkHyphens: Boolean
  def checkBidi: Boolean
  def checkJoiners: Boolean
  def useStd3ASCIIRules: Boolean
  def transitionalProcessing: Boolean
  def verifyDnsLength: Boolean

  def withCheckHyphens(value: Boolean): UTS46Config
  def withCheckBidi(value: Boolean): UTS46Config
  def withCheckJoiners(value: Boolean): UTS46Config
  def withUseStd3ASCIIRules(value: Boolean): UTS46Config
  def withTransitionalProcessing(value: Boolean): UTS46Config
  def withVerifyDNSLength(value: Boolean): UTS46Config

  override final def toString: String =
    s"UTS46Config(checkHyphens = ${checkHyphens}, checkBidi = ${checkBidi}, checkJoiners = ${checkJoiners}, useStd3ASCIIRules = ${useStd3ASCIIRules}, transitionalProcessing = ${transitionalProcessing}, verifyDnsLength = ${verifyDnsLength})"
}

object UTS46Config {
  private[this] final case class UTS46ConfigImpl(override val checkHyphens: Boolean, override val checkBidi: Boolean, override val checkJoiners: Boolean, override val useStd3ASCIIRules: Boolean, override val transitionalProcessing: Boolean, override val verifyDnsLength: Boolean) extends UTS46Config {
    override def withCheckHyphens(value: Boolean): UTS46Config =
      copy(checkHyphens = value)
    override def withCheckBidi(value: Boolean): UTS46Config =
      copy(checkBidi = value)
    override def withCheckJoiners(value: Boolean): UTS46Config =
      copy(checkJoiners = value)
    override def withUseStd3ASCIIRules(value: Boolean): UTS46Config =
      copy(useStd3ASCIIRules = value)
    override def withTransitionalProcessing(value: Boolean): UTS46Config =
      copy(transitionalProcessing = value)
    override def withVerifyDNSLength(value: Boolean): UTS46Config =
      copy(verifyDnsLength = value)
  }

  def apply(
    checkHyphens: Boolean,
    checkBidi: Boolean,
    checkJoiners: Boolean,
    useStd3ASCIIRules: Boolean,
    transitionalProcessing: Boolean,
    verifyDnsLength: Boolean
  ): UTS46Config =
    UTS46ConfigImpl(
      checkHyphens,
      checkBidi,
      checkJoiners,
      useStd3ASCIIRules,
      transitionalProcessing,
      verifyDnsLength
    )
}
