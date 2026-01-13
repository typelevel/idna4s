/*
 * Copyright 2022 Typelevel
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

import cats.data._
import cats.syntax.all._
import sbt._

/**
 * Entry point for running code generation.
 *
 * This object provides one public method `generate` which in turn invokes all the other code
 * generation methods. The intent is to make it simple to invoke this from `build.sbt` and
 * ensure that all the code generation is using a consistent set of inputs, in particular the
 * UnicodeVersion.
 */
object CodeGen {

  /**
   * Run all the code generation
   *
   * @param baseDir
   *   the directory where the generated code files will be written, usually `(Compile /
   *   sourceManaged)`.
   * @param unicodeVersion
   *   the version of Unicode to use to generate the code.
   */
  def generate(baseDir: File, unicodeVersion: String): Seq[File] = {
    val parsedUnicodeVersion = UnicodeVersion.unsafeFromString(unicodeVersion)
    NonEmptyList
      .of(
        UTS46CodeGen.generate(baseDir, parsedUnicodeVersion),
        UnicodeDataCodeGen.generate(baseDir, parsedUnicodeVersion),
        DerivedJoiningTypeCodeGen.generate(baseDir, parsedUnicodeVersion),
        DerivedBidiClassCodeGen.generate(baseDir, parsedUnicodeVersion)
      )
      .reduceMap(
        _.fold(
          e => Ior.leftNec[Throwable, NonEmptyList[File]](e),
          f => Ior.right(NonEmptyList.one(f))
        )
      )
      .fold(
        e => throw collapseErrors(e),
        _.toList,
        { case (e, _) => throw collapseErrors(e) }
        )
  }

  /**
   * Simple method to collapse all errors we encounter during code generation into a single
   * exception.
   */
  private def collapseErrors(e: NonEmptyChain[Throwable]): RuntimeException =
    new RuntimeException(s"""Encountered ${e.size} errors during code generation: ${e
        .map(_.getLocalizedMessage)
        .mkString_(", ")}""")
}
