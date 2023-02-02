package wacc.frontend

import parsley.Parsley
import Parsley._
import parsley.implicits.character.stringLift
import parsley.errors.combinator.ErrorMethods
import parsley.token.Lexer
import parsley.token.descriptions.{LexicalDesc, NameDesc, SpaceDesc, SymbolDesc}
import parsley.token.descriptions.numeric.{ExponentDesc, NumericDesc}
import parsley.token.descriptions.text.{EscapeDesc, TextDesc}
import parsley.token.predicate

object Lexer {
  private val waccDesc = LexicalDesc.plain.copy(
    spaceDesc = SpaceDesc.plain.copy(
      commentLine = "#"
    ),
    symbolDesc = SymbolDesc.plain.copy(
      hardKeywords = Set(
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
        "null"
      ),
      hardOperators = Set(
        "!",
        "-",
        "len",
        "ord",
        "chr",
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
      graphicCharacter =
        predicate.Basic(c => c >= ' ' && !Set('\\', '\'', '\"').contains(c))
    )
  )
  private val lexer = new Lexer(waccDesc)

  val VAR_ID: Parsley[String] = lexer.lexeme.names.identifier

  val INTEGER: Parsley[Int] =
    lexer.lexeme.numeric.integer.decimal32.label("integer")
  val BOOL: Parsley[Boolean] =
    lexer.lexeme(attempt("true" #> true <|> "false" #> false)).label("boolean")
  val STRING: Parsley[String] =
    lexer.lexeme.text.string.ascii.label("string literal")
  val CHAR: Parsley[Char] =
    lexer.lexeme.text.character.ascii.label("char literal")

  def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)

  val implicits = lexer.lexeme.symbol.implicits

}
