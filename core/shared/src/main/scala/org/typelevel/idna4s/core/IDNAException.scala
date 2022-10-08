package org.typelevel.idna4s.core

import cats._

/**
 * Marker trait for IDNA errors.
 *
 * Errors in idna4s are usually scoped to sub-domains, e.g. a class of errors for Bootstring, a
 * class of errors for UTS-46 code point mapping, etc. In many cases we are mandated by the
 * relevant specifications to return an error ''and'' continue processing, so we often end up in
 * a situation where we have a partial result and more than one error.
 *
 * We use the [[IDNAException]] marker trait so that we can express certain errors in their
 * domain specific representation, but still aggregate different domains together, e.g. `((a:
 * NonEmptyList[BootStringException]).widen ++ (b: NonEmptyList[MappingException]).widen):
 * NonEmptyList[IDNAException]`.
 */
trait IDNAException extends RuntimeException

object IDNAException {
  implicit val showForIDNAException: Show[IDNAException] =
    Show.fromToString
}
