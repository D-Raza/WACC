package wacc.frontend

import parsley.Parsley
import parsley.Parsley._
import parsley.character.digit
import parsley.errors.combinator._
import parsley.implicits.character.stringLift
import parsley.token.descriptions.numeric.{ExponentDesc, NumericDesc}
import parsley.token.descriptions.text.{EscapeDesc, TextDesc}
import parsley.token.descriptions.{LexicalDesc, NameDesc, SpaceDesc, SymbolDesc}
import parsley.token.{Lexer, predicate}

object Lexer {
  final val keywords = Set(
    "begin",
    "end",
    "is",
    "skip",
    "read",
    "free",
    "return",
    "exit",
    "print",
    "println",
    "if",
    "then",
    "else",
    "fi",
    "while",
    "do",
    "done",
    "newpair",
    "call",
    "fst",
    "snd",
    "int",
    "bool",
    "char",
    "string",
    "pair",
    "true",
    "false",
    "null",
    "len",
    "ord",
    "chr"
  )
  val waccDesc = LexicalDesc.plain.copy(
    spaceDesc = SpaceDesc.plain.copy(
      commentLine = "#",
      space = predicate.Basic(_.isWhitespace)
    ),
    symbolDesc = SymbolDesc.plain.copy(
      hardKeywords = keywords,
      hardOperators = Set(
        "!",
        "-",
        "*",
        "/",
        "%",
        "+",
        ">",
        ">=",
        "<",
        "<=",
        "==",
        "!=",
        "&&",
        "||"
      )
    ),
    nameDesc = NameDesc.plain.copy(
      identifierStart = predicate.Basic(c => Character.isLetter(c) || c == '_'),
      identifierLetter =
        predicate.Basic(c => Character.isLetterOrDigit(c) || c == '_')
    ),
    numericDesc = NumericDesc.plain.copy(
      integerNumbersCanBeHexadecimal = false,
      integerNumbersCanBeOctal = false,
      decimalExponentDesc = ExponentDesc.NoExponents
    ),
    textDesc = TextDesc.plain.copy(
      escapeSequences = EscapeDesc.plain.copy(
        escBegin = '\\',
        literals = Set.empty,
        singleMap = Map(
          '0' -> 0x0000,
          'b' -> 0x0008,
          't' -> 0x0009,
          'n' -> 0x000a,
          'f' -> 0x000c,
          'r' -> 0x000d,
          '"' -> 0x0022,
          '\'' -> 0x0027,
          '\\' -> 0x005c
        )
      ),
      graphicCharacter = predicate.Basic(c =>
        c >= ' ' && c <= '~' && !Set('\\', '\'', '\"').contains(c)
      )
    )
  )
  val lexer = new Lexer(waccDesc)
  val VAR_ID: Parsley[String] =
    token {
      amend {
        entrench(lexer.lexeme.names.identifier).map(_.mkString).filterOut {
          case v if keywords(v) =>
            s"keyword $v may not be used as an identifier"
        }
      }
    }.label("identifier")
  val NEGATE: Parsley[Unit] =
    lexer.lexeme(attempt("-" *> notFollowedBy(digit))).hide
  val INTEGER: Parsley[Int] =
    lexer.lexeme.numeric.integer.decimal32.label("integer literal")
  val BOOL: Parsley[Boolean] =
    lexer
      .lexeme(attempt("true" #> true <|> "false" #> false))
      .label("boolean literal")
  val STRING: Parsley[String] =
    lexer.lexeme.text.string.ascii.label("string literal")
  val CHAR: Parsley[Char] =
    lexer.lexeme.text.character.ascii.label("char literal")
  val implicits = lexer.lexeme.symbol.implicits

  def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)

  private def token[A](p: Parsley[A]): Parsley[A] =
    lexer.lexeme(attempt(p))

}
