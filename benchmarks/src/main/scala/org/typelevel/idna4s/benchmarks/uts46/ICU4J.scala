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

package org.typelevel.idna4s.benchmarks.uts46

import cats.syntax.all._
import com.ibm.icu.text.Normalizer2
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import org.scalacheck._
import org.typelevel.idna4s.core.uts46._

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class ICU4J {
  import ICU4J._

  var currentSeed: Long = Long.MinValue // scalafix:ok

  def nextSeed: Long = {
    val s: Long = currentSeed
    currentSeed = currentSeed + 1L
    s
  }

  def next[A](gen: Gen[A], seed: Long): A =
    gen(Gen.Parameters.default, rng.Seed(seed)).getOrElse(throw new AssertionError)

  def nextASCIIString: String =
    next[String](Gen.asciiStr, nextSeed)

  def nextString: String =
    next[String](genString, nextSeed)

  @Benchmark
  def idna4sUTS46: Either[CodePointMapper.MappingException, String] =
    CodePointMapper.mapCodePoints(nextString)

  @Benchmark
  def icu4jUTS46: String =
    icu4jUTS46Normalizer2
      .normalize(nextString, new java.lang.StringBuilder(nextString.size))
      .toString

  @Benchmark
  def idna4sUTS46ASCII: Either[CodePointMapper.MappingException, String] =
    CodePointMapper.mapCodePoints(nextASCIIString)

  @Benchmark
  def icu4jUTS46ASCII: String =
    icu4jUTS46Normalizer2
      .normalize(nextASCIIString, new java.lang.StringBuilder(nextString.size))
      .toString
}

object ICU4J {
  val genString: Gen[String] =
    Arbitrary.arbitrary[String]

  val icu4jUTS46Normalizer2: Normalizer2 =
    Normalizer2.getInstance(null, "uts46", Normalizer2.Mode.COMPOSE)
}
