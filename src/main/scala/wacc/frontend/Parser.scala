package wacc.frontend

import wacc.frontend.Lexer._
import java.io.File
import parsley.io.ParseFromIO
import parsley.{Parsley, Result}
import parsley.character.digit
import parsley.expr.chain
import parsley.implicits.character.charLift

object Parser {
  private lazy val integer =
    digit.foldLeft1[BigInt](0)((n, d) => n * 10 + d.asDigit)

  private val add = (x: BigInt, y: BigInt) => x + y
  private val sub = (x: BigInt, y: BigInt) => x - y

  private lazy val expr: Parsley[BigInt] =
    chain.left1[BigInt](
      ('(' ~> expr <~ ')') <|> integer,
      ('+' #> add) <|> ('-' #> sub)
    )

  private lazy val program = fully(expr)

  def parse(input: File): Result[String, BigInt] =
    program.parseFromFile(input).get
}
