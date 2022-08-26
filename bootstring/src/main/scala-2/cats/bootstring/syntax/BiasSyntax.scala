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

package org.typelevel.idna4s.bootstring.syntax

import org.typelevel.idna4s.bootstring._
import org.typelevel.literally.Literally

private[syntax] trait BiasSyntax {
  implicit class BiasContext(val sc: StringContext) {
    def bias(args: Any*): Bias = macro BiasSyntax.bias.make
  }
}

private object BiasSyntax {

  private object bias extends Literally[Bias] {
    def validate(c: Context)(s: String): Either[String, c.Expr[Bias]] = {
      import c.universe._

      Bias.fromString(s).map(_ => c.Expr(q"Bias.unsafeFromString($s)"))
    }

    def make(c: Context)(args: c.Expr[Any]*): c.Expr[Bias] =
      apply(c)(args: _*)
  }
}
