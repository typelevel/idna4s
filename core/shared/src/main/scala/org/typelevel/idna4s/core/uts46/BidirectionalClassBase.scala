package org.typelevel.idna4s.core.uts46

/**
 * Base class used with code generation. The code which does the generation should be found in
 * the `project/` directory of the sbt build. The generated implementation is expected to be
 * named `GeneratedBidirectionalClass`.
 *
 * Values in this class come from `DerivedBidiClass.txt`
 *
 * @see
 *   [[https://www.unicode.org/Public/15.0.0/ucd/extracted/DerivedBidiClass.txt]]
 */
private[idna4s] trait BidirectionalClassBase {

  /** Get the bidirectional class value for a the given code point.
    *
    * This method only valid for `Int` values which are valid code points. If
    * a `Int` value which is not a code point is passed in, this method will
    * throw. For this reason, this method should not be made public.
    *
    * The resulting `String` is the bidirectional ''alias'' string. For example,
    * bidirectional class named "Left_To_Right", the bidirectional class alias
    * is "L".
    *
    * @note Bidirectional class alias values are ''not'' only of length 1. For
    * example, "Other_Neutral" is represented by the string "ON"
    */
  protected def bidiTypeForCodePointInt(cp: Int): String
}
