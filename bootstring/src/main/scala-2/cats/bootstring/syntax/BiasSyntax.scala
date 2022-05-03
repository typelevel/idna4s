package cats.bootstring.syntax

import cats.bootstring._
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
