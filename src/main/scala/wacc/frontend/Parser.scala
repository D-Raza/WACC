package wacc.frontend

import parsley.Parsley._
import parsley.combinator.{sepBy, sepBy1, some, many}
import parsley.errors.combinator._
import parsley.expr._
import parsley.io.ParseFromIO
import parsley.{Parsley, Result}
import wacc.AST._
import wacc.frontend.Errors._
import wacc.frontend.Lexer._
import wacc.frontend.Lexer.implicits.implicitSymbol

import java.io.File

object Parser {

  // Utilise our custom error builder
  implicit val waccErrorBuilder: WACCErrorBuilder =
    new WACCErrorBuilder

  /* <source-file> ::= (<import>)* <program> */
  private lazy val `<source-file>` = fully(
    SourceFile(many(`<import>`), `<program>`)
  )

  /* <import> ::= "import" <string-literal> */
  private lazy val `<import>` = Import("import" *> STRING)

  /* <program> ::= "begin" (<func>)* <stat> (";" <stat>)* "end" */
  private lazy val `<program>` = fully(
    "begin" *> (attempt(Program(pure(Nil), sepBy1(`<stat>`, ";"))) <|>
      Program(some(`<func>`), sepBy1(`<stat>`, ";"))) <* "end"
  )

  // <func> ::= = <type> <ident> ‘(’ <param-list>? ‘)’ ‘is’ <stat> ‘end’
  private lazy val `<func>` = attempt(
    Func(
      `<type>`.explain("function declaration missing return type"),
      `<ident>`,
      "(" *> `<param-list>` <* ")",
      "is" *> sepBy1(`<stat>`, ";").guardAgainst(statsMissingRet) <* "end"
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
               | "println" <expr>
               | "if" <expr> "then" <stat> "else" <stat> "fi"
               | "while" <expr> "do" <stat> "done"
               | "begin" <stat> "end" */
  private lazy val `<stat>` : Parsley[Stat] = amend {
    entrench(
      Skip <# "skip".label("skip")
        <|> Declare(`<type>`, `<ident>`, "=".label("declaration") *> `<rvalue>`)
        <|> Assign(`<lvalue>`, "=".label("assignment") *> `<rvalue>`)
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
    )
  }
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
  private lazy val `<type>` : Parsley[Type] = chain
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
    "pair" *> "(" *> `<type>` <* ",",
    `<type>` <* ")"
  )
  // <pair-elem-type> ::= <base-type> | <array-type> | "pair"
  // private lazy val `<pair-elem-type>` : Parsley[PairElemType] = attempt(
  //   chain.postfix1(`<base-type>` <|> `<pair-type>`, `<array-type>`)
  // ) <|> `<base-type>` <|> (InnerPairType <# "pair")
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
    ArrayElem(
      `<ident>`,
      some("[".label("index (like `xs[idx]`)") *> `<expr>` <* "]")
    )
      .label("array element")
  // <array-liter> ::= '[' (<expr> (',' <expr>)*)? ']'
  private lazy val `<array-liter>` = ArrayLit(
    "[" *> sepBy(`<expr>`, ",") <* "]"
  ).label("array literal")
  private val statsMissingRet = new PartialFunction[List[Stat], Seq[String]] {

    private def statsMissingRet(stats: List[Stat]): Boolean = {
      stats.last match {
        case Return(_) | Exit(_) => false
        case Scope(scopedStats)  => statsMissingRet(scopedStats)
        case If(_, thenStats, elseStats) =>
          statsMissingRet(thenStats) || statsMissingRet(elseStats)
        case While(_, doStats) => statsMissingRet(doStats)
        case _                 => true
      }
    }

    override def apply(stats: List[Stat]): Seq[String] = {
      Seq("function is missing a return on all exit paths")
    }

    override def isDefinedAt(stats: List[Stat]): Boolean = {
      statsMissingRet(stats)
    }
  }

  /** Parses a WACC program from a file
    * @param input
    *   the file to parse
    * @return
    *   the parsed program
    */
  def parse(input: File): Result[WACCError, SourceFile] =
    `<source-file>`.parseFromFile(input).get

}
