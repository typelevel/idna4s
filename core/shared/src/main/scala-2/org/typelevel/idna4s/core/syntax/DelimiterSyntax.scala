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

package org.typelevel.idna4s.core.syntax

import org.typelevel.idna4s.core.bootstring._
import org.typelevel.literally.Literally

private[syntax] trait DelimiterSyntax {
  implicit class DelimiterContext(val sc: StringContext) {
    def delimiter(args: Any*): Delimiter = macro DelimiterSyntax.delimiter.make
  }
}

private object DelimiterSyntax {

  private object delimiter extends Literally[Delimiter] {
    def validate(c: Context)(s: String): Either[String, c.Expr[Delimiter]] = {
      import c.universe._

      Delimiter.fromString(s).map(_ => c.Expr(q"Delimiter.unsafeFromString($s)"))
    }

    def make(c: Context)(args: c.Expr[Any]*): c.Expr[Delimiter] =
      apply(c)(args: _*)
  }
}
