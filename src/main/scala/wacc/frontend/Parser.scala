package wacc.frontend

import wacc.frontend.Lexer._
import wacc.frontend.Lexer.implicits.implicitSymbol
import wacc.AST._
import wacc.frontend.errors._
import java.io.File
import parsley.Parsley._
import parsley.{Parsley, Result}
import parsley.errors.combinator._
import parsley.combinator.{some, many, sepBy, sepBy1}
import parsley.expr._
import parsley.io.ParseFromIO

object Parser {

  implicit val waccErrorBuilder: WACCErrorBuilder =
    new WACCErrorBuilder

  def parse(input: File): Result[WACCError, Program] =
    `<program>`.parseFromFile(input).get

  // <program> ::= 'begin' <func>* <stat> 'end'
  private lazy val `<program>` = fully(
    "begin" *> Program(
      many(`<func>`),
      sepBy1(`<stat>`, ";") <* "end"
    )
  )

  private def missingFuncRet(stats: List[Stat]): Boolean = {
    stats.last match {
      case Return(_) | Exit(_) => false
      case Scope(scopedStats)  => missingFuncRet(scopedStats)
      case If(_, thenStats, elseStats) =>
        missingFuncRet(thenStats) || missingFuncRet(elseStats)
      case While(_, doStats) => missingFuncRet(doStats)
      case _                 => true
    }
  }

  // <func> ::= = <type> <ident> ‘(’ <param-list>? ‘)’ ‘is’ <stat> ‘end’
  private lazy val `<func>` = attempt(
    Func(
      `<type>`,
      `<ident>`,
      "(" *> `<param-list>` <* ")",
      "is" *> sepBy1(`<stat>`, ";").filterNot(missingFuncRet) <* "end"
    )
  )

  // <param-list> ::= <param> (',' <param>)*
  private lazy val `<param-list>` = sepBy(`<param>`, ",")

  // <param> ::= <type> <ident>
  private lazy val `<param>` = Param(`<type>`, `<ident>`)

  /* <stat> ::= "skip"
               | <type> <ident> "=" <expr>
               | <ident> "=" <expr>
               | "read" <ident>
               | "free" <ident>
               | "return" <expr>
               | "exit" <expr>
               | "print" <expr>
               | "println" <expr>
               | "if" <expr> "then" <stat> "else" <stat> "fi"
               | "while" <expr> "do" <stat> "done"
               | "begin" <stat> "end"
               | <stat> ";" <stat> */
  private lazy val `<stat>` : Parsley[Stat] = (
    Skip <# "skip".label("skip")
      <|> Declare(`<type>`, `<ident>`, "=" *> `<rvalue>`)
      <|> Assign(`<lvalue>`, "=" *> `<rvalue>`)
      <|> Read("read" *> `<lvalue>`).label("read")
      <|> Free("free" *> `<expr>`).label("free")
      <|> Return("return" *> `<expr>`).label("return")
      <|> Exit("exit" *> `<expr>`).label("exit")
      <|> Print("print" *> `<expr>`).label("print")
      <|> Println("println" *> `<expr>`).label("println")
      <|> If(
        "if" *> `<expr>`,
        "then" *> sepBy1(`<stat>`, ";"),
        "else" *> sepBy1(`<stat>`, ";") <* "fi"
      ).label("if statement")
      <|> While(
        "while" *> `<expr>`,
        "do" *> sepBy1(`<stat>`, ";") <* "done".explain("unclosed while loop")
      ).label("while loop")
      <|> Scope(
        "begin" *> sepBy1(`<stat>`, ";") <* "end"
      )
  ) // .label("statement")

  // <lvalue> ::= <ident> | <array-elem> | <pair-elem>
  private lazy val `<lvalue>` : Parsley[LValue] = (
    attempt(`<array-elem>`)
      <|> `<pair-elem>`
      <|> `<ident>`
  )

  // <pair-elem> ::= "fst" <lvalue> | "snd" <lvalue>
  private lazy val `<pair-elem>` =
    Fst("fst" *> `<lvalue>`) <|> Snd("snd" *> `<lvalue>`)

  // <rvalue> ::= <expr> | <array-liter> | 'newpair' '('' <expr> ',' <expr> ')' | `<pair-elem>` | 'call' <ident> '(' <arg-list> ')'
  private lazy val `<rvalue>` = (
    `<expr>`
      <|> `<array-liter>`
      <|> NewPair("newpair" *> "(" *> `<expr>` <* ",", `<expr>` <* ")")
        .label("pair instantiation")
      <|> `<pair-elem>`.label("pair element").explain(
        "pair elements may be accessed with fst or snd"
      )
      <|> Call("call" *> `<ident>`, "(" *> `<arg-list>` <* ")")
        .label("function call")
  )

  // <arg-list> ::= <expr> (‘,’ <expr>)*
  private lazy val `<arg-list>` = sepBy(`<expr>`, ",")

  // <type> ::= <base-type> | <array-type> | <pair-type>
  private lazy val `<type>` = chain
    .postfix(`<base-type>` <|> `<pair-type>`, `<array-type>`)
    .label("type")
    .explain(
      "valid types are int, bool, char, string, pair, and arrays of any of these types"
    )

  // <base-type> ::= 'int' | 'bool' | 'char' | 'string'
  private lazy val `<base-type>` = (
    (IntType <# "int")
      <|> (BoolType <# "bool")
      <|> (CharType <# "char")
      <|> (StringType <# "string")
  )

  // <array-type> ::= <type> '[' ']'
  private lazy val `<array-type>` = ArrayType <# "[]".label("[] (array type)")

  // <pair-type> ::= ‘pair’ ‘(’ <pair-elem-type> ‘,’ <pair-elem-type> ‘)’
  private lazy val `<pair-type>` = PairType(
    "pair" *> "(" *> `<pair-elem-type>` <* ",",
    `<pair-elem-type>` <* ")"
  )

  // <pair-elem-type> ::= <base-type> | <array-type> | "pair"
  private lazy val `<pair-elem-type>` : Parsley[PairElemType] = attempt(
    chain.postfix1(`<base-type>` <|> `<pair-type>`, `<array-type>`)
  ) <|> `<base-type>` <|> (InnerPairType <# "pair")

  private lazy val `<expr>` : Parsley[Expr] = precedence(
    SOps(InfixR)(Or <# "||".label("binary operator")) +:
      SOps(InfixR)(And <# "&&".label("binary operator")) +:
      SOps(InfixN)(
        Equal <# "==".label("binary operator"),
        NotEqual <# "!=".label("binary operator")
      )
      +: SOps(InfixN)(
        LT <# "<".label("binary operator"),
        LTE <# "<=".label("binary operator"),
        GT <# ">".label("binary operator"),
        GTE <# ">=".label("binary operator")
      )
      +: SOps(InfixL)(
        Add <# "+".label("binary operator"),
        Sub <# "-".label("binary operator")
      )
      +: SOps(InfixL)(
        Mult <# "*".label("binary operator"),
        Div <# "/".label("binary operator"),
        Mod <# "%".label("binary operator")
      )
      +: SOps(Prefix)(
        Not <# "!".label("unary operator"),
        Neg <# NEGATE.label("unary operator"),
        Len <# "len".label("unary operator"),
        Ord <# "ord".label("unary operator"),
        Chr <# "chr".label("unary operator")
      )
      +: Atoms(
        IntegerLiter(INTEGER),
        BoolLiter(BOOL),
        CharLiter(CHAR),
        StrLiter(STRING),
        attempt(`<array-elem>`),
        `<ident>`,
        Bracket("(" *> `<expr>` <* ")"),
        Null <# "null"
      )
  )

  // <ident> ::= (‘_’ | ‘a’-‘z’ | ‘A’-‘Z’) (‘–’ | ‘a’-‘z’ | ‘A’-‘Z’ | ‘0’-‘9’)*
  private lazy val `<ident>` = Ident(VAR_ID)

  // <array-elem> ::= <ident> ('[' <expr> ']')+
  private lazy val `<array-elem>` =
    ArrayElem(`<ident>`, some("[" *> `<expr>`.label("index") <* "]"))
      .label("array element")

  // <array-liter> ::= '[' (<expr> (',' <expr>)*)? ']'
  private lazy val `<array-liter>` = ArrayLit(
    "[" *> sepBy(`<expr>`, ",") <* "]"
  ).label("array literal")

}
