package wacc.frontend

import parsley.Parsley
import parsley.token.Lexer
import parsley.token.predicate.Basic
import parsley.token.descriptions.{LexicalDesc, NameDesc, SymbolDesc, SpaceDesc}
import parsley.token.descriptions.numeric.{NumericDesc, ExponentDesc}
import parsley.token.descriptions.text.{TextDesc, EscapeDesc}

object Lexer {
  private val waccDesc = LexicalDesc.plain.copy(
    spaceDesc = SpaceDesc.plain.copy(
      commentLine = "#",
      space = Basic(c => c.isWhitespace)
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
        "len",
        "ord",
        "chr",
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
      ),
      caseSensitive = true
    ),
    nameDesc = NameDesc.plain.copy(
      identifierStart = Basic(c => Character.isLetter(c) || c == '_'),
      identifierLetter = Basic(c => Character.isLetterOrDigit(c) || c == '_')
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
      graphicCharacter = Basic(c =>
        c != '"' && c != '\'' && c != '\\' && c.toInt >= ' '.toInt && c.toInt <= '~'.toInt
      )
    )
  )
  private val lexer = new Lexer(waccDesc)

  def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}
