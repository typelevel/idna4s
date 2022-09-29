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

import cats.syntax.all._
import org.typelevel.idna4s.core.bootstring._
import scala.language.future
import scala.quoted.*

private[syntax] trait DelimiterSyntax {
  extension (inline ctx: StringContext) {
    inline def delimiter(inline args: Any*): Delimiter =
      DelimiterSyntax.delimiterLiteral(ctx, args)
  }
}

private object DelimiterSyntax {

  private def delimiterLiteralExpr(sc: Expr[StringContext], args: Expr[Seq[Any]])(
      using q: Quotes): Expr[Delimiter] =
    sc.value match {
      case Some(sc) if sc.parts.size === 1 =>
        val value: String = sc.parts.head
        Delimiter
          .fromString(value)
          .fold(
            e => {
              quotes.reflect.report.errorAndAbort(e)
            },
            _ => '{ Delimiter.unsafeFromString(${ Expr(value) }) }
          )
      case Some(_) =>
        quotes.reflect.report.errorAndAbort("StringContext must be a single string literal")
      case None =>
        quotes.reflect.report.errorAndAbort("StringContext args must be statically known")
    }

  inline def delimiterLiteral(inline sc: StringContext, inline args: Any*): Delimiter =
    ${ delimiterLiteralExpr('sc, 'args) }
}
