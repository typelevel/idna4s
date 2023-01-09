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

  /**
   * Get the bidirectional class value for a the given code point.
   *
   * This method only valid for `Int` values which are valid code points. If a `Int` value which
   * is not a code point is passed in, this method will throw. For this reason, this method
   * should not be made public.
   *
   * The resulting `String` is the bidirectional ''alias'' string. For example, bidirectional
   * class named "Left_To_Right", the bidirectional class alias is "L".
   *
   * @note
   *   Bidirectional class alias values are ''not'' only of length 1. For example,
   *   "Other_Neutral" is represented by the string "ON"
   */
  protected def bidiTypeForCodePointInt(cp: Int): String
}
