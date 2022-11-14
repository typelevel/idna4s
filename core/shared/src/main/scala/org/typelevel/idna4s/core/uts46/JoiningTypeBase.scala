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

import scala.collection.immutable.IntMap

/**
 * Base class used with code generation. The code which does the generation should be found in
 * the `project/` directory of the sbt build. The generated implementation is expected to be
 * named `GeneratedJoiningType`.
 *
 * Values in this class come from `DerivedJoiningType.txt`.
 *
 * @see
 *   [[https://www.unicode.org/Public/15.0.0/ucd/extracted/DerivedJoiningType.txt]]
 */
private[uts46] trait JoiningTypeBase {

  /**
   * The character value used to indicate that a code point is non-joining.
   */
  final private val NonJoining = 'U'

  /**
   * An `IntMap` of code points to joining types. This should be implemented by generated code.
   * It will ''not'' contain mappings for code points which are non-joining (the vast majority
   * of code points).
   */
  protected def joiningTypeMap: IntMap[Char]

  /**
   * For a given code point, retrieve the character that Unicode uses to indicate the joining
   * type.
   *
   * @note
   *   For performance reasons, we do not check to see if the passed in int32 value is a valid
   *   Unicode code point. It is expected this will be called only from methods which have
   *   already validated that the int32 values in scope are valid code points.
   */
  final protected def joiningTypeForCodePoint(codePoint: Int): Char =
    joiningTypeMap.getOrElse(codePoint, NonJoining)

  // UTS-46 only cares about a specific set of Joining Types. The code
  // technically handles any joining type because it is a relatively small set
  // of code points in total that have joining types and so it was decided
  // that the most simple solution would be to just generate a mapping for all
  // code points.
  //
  // The methods below are specialized to check to see if a code point has a
  // joining type which is interesting to UTS-46.

  /**
   * For a given Unicode code point, check to see if the has the given joining type.
   */
  @SuppressWarnings(Array("scalafix:DisableSyntax.=="))
  final protected def isJoiningType(codePoint: Int, joiningType: Char): Boolean =
    joiningTypeForCodePoint(codePoint) == joiningType

  /**
   * For a given Unicode code point, check to see if it is left joining.
   */
  final protected def isJoiningTypeL(codePoint: Int): Boolean =
    isJoiningType(codePoint, 'L')

  /**
   * For a given Unicode code point, check to see if it is dual joining.
   */
  final protected def isJoiningTypeD(codePoint: Int): Boolean =
    isJoiningType(codePoint, 'D')

  /**
   * For a given Unicode code point, check to see if it has transparent joining.
   */
  final protected def isJoiningTypeT(codePoint: Int): Boolean =
    isJoiningType(codePoint, 'T')

  /**
   * For a given Unicode code point, check to see if it is right joining.
   */
  final protected def isJoiningTypeR(codePoint: Int): Boolean =
    isJoiningType(codePoint, 'R')
}
