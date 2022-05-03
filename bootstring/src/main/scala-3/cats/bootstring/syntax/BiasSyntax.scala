package cats.bootstring.syntax

import cats.bootstring._
import scala.language.future
import scala.quoted.*

private[syntax] trait BiasSyntax {
  extension (inline ctx: StringContext) {
    inline def bias(inline args: Any*): Bias =
      BiasSyntax.biasLiteral(ctx, args)
  }
}

private object BiasSyntax {

  private def biasLiteralExpr(sc: Expr[StringContext], args: Expr[Seq[Any]])(
      using q: Quotes): Expr[Bias] =
    sc.value match {
      case Some(sc) if sc.parts.size == 1 =>
        val value: String = sc.parts.head
        Bias
          .fromString(value)
          .fold(
            e => {
              quotes.reflect.report.throwError(e)
            },
            _ => '{ Bias.unsafeFromString(${ Expr(value) }) }
          )
      case Some(_) =>
        quotes.reflect.report.throwError("StringContext must be a single string literal")
      case None =>
        quotes.reflect.report.throwError("StringContext args must be statically known")
    }

  inline def biasLiteral(inline sc: StringContext, inline args: Any*): Bias =
    ${ biasLiteralExpr('sc, 'args) }
}
