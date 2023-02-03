package wacc.frontend

import wacc.frontend.Lexer._
import wacc.frontend.Lexer.implicits.implicitSymbol
import wacc.AST._
import java.io.File
import parsley.Parsley._
import parsley.{Parsley, Result}
import parsley.combinator.{some, many, sepBy, sepBy1}
import parsley.expr._
import parsley.io.ParseFromIO

object Parser {
  def parse(input: File): Result[String, Program] =
    `<program>`.parseFromFile(input).get

  private lazy val `<program>` = fully(
    "begin" *> Program(
      many(`<func>`),
      sepBy1(`<stat>`, ";") <* "end"
    )
  )

  private lazy val `<func>` = attempt(
    Func(
      `<type>`,
      `<ident>`,
      "(" *> `<param-list>` <* ")",
      "is" *> sepBy1(`<stat>`, ";") <* "end"
    )
  )

  private lazy val `<param-list>` = sepBy(`<param>`, ",")

  private lazy val `<param>` = Param(`<type>`, `<ident>`)

  private lazy val `<stat>` : Parsley[Stat] = (
    Skip <# "skip"
      <|> Declare(`<type>`, `<ident>`, "=" *> `<rvalue>`)
      <|> Assign(`<lvalue>`, "=" *> `<rvalue>`)
      <|> Read("read" *> `<lvalue>`)
      <|> Free("free" *> `<expr>`)
      <|> Return("return" *> `<expr>`)
      <|> Exit("exit" *> `<expr>`)
      <|> Print("print" *> `<expr>`)
      <|> Println("println" *> `<expr>`)
      <|> If(
        "if" *> `<expr>`,
        "then" *> sepBy1(`<stat>`, ";"),
        "else" *> sepBy1(`<stat>`, ";") <* "fi"
      )
      <|> While(
        "while" *> `<expr>`,
        "do" *> sepBy1(`<stat>`, ";") <* "done"
      )
      <|> Scope(
        "begin" *> sepBy1(`<stat>`, ";") <* "end"
      )
  )

  private lazy val `<lvalue>` : Parsley[LValue] = (
    `<ident>`
      <|> `<array-elem>`
      <|> `<pair-elem>`
  )

  private lazy val `<pair-elem>` =
    Fst("fst" *> `<lvalue>`) <|> Snd("snd" *> `<lvalue>`)

  private lazy val `<rvalue>` = (
    `<expr>`
      <|> `<array-liter>`
      <|> NewPair("newpair" *> "(" *> `<expr>` <* ",", `<expr>` <* ")")
      <|> `<pair-elem>`
      <|> Call("call" *> `<ident>`, "(" *> `<arg-list>` <* ")")
  )

  private lazy val `<arg-list>` = sepBy(`<expr>`, ",")

  private lazy val `<type>` = chain
    .postfix(`<base-type>` <|> `<pair-type>`, `<array-type>`)

  private lazy val `<base-type>` = (
    (IntType <# "int")
      <|> (BoolType <# "bool")
      <|> (CharType <# "char")
      <|> (StringType <# "string")
  )

  private lazy val `<array-type>` = ArrayType <# ("[" <* "]")

  private lazy val `<pair-type>` = PairType(
    "pair" *> "(" *> `<pair-elem-type>` <* ",",
    `<pair-elem-type>` <* ")"
  )

  private lazy val `<pair-elem-type>` : Parsley[PairElemType] = attempt(
    chain.postfix1(`<base-type>` <|> `<pair-type>`, ArrayType <# ("[" <* "]"))
  ) <|> `<base-type>` <|> (InnerPairType <# "pair")

  private lazy val `<expr>` : Parsley[Expr] = precedence(
    SOps(InfixL)(Or <# "||") +:
      SOps(InfixL)(And <# "&&") +:
      SOps(InfixL)(
        Equal <# "==",
        NotEqual <# "!="
      )
      +: SOps(InfixL)(
        LT <# "<",
        LTE <# "<=",
        GT <# ">",
        GTE <# ">="
      )
      +: SOps(InfixL)(
        Add <# "+",
        Sub <# "-"
      )
      +: SOps(InfixL)(
        Mult <# "*",
        Div <# "/",
        Mod <# "%"
      )
      +: SOps(Prefix)(
        Not <# "!",
        Negate <# "-",
        Len <# "len",
        Ord <# "ord",
        Chr <# "chr"
      )
      +: Atoms(
        IntegerLiter(INTEGER),
        BoolLiter(BOOL),
        CharLiter(CHAR),
        StrLiter(STRING),
        `<array-elem>`,
        `<ident>`,
        Bracket("(" *> `<expr>` <* ")")
      )
  )

  private lazy val `<ident>` = Ident(VAR_ID)

  private lazy val `<array-elem>` =
    attempt(
      ArrayElem(`<ident>`, some("[" *> `<expr>` <* "]"))
    ) // TODO: is this correct?

  private lazy val `<array-liter>` = ArrayLit(
    ("[" *> sepBy(`<expr>`, ",") <* "]")
  )

}
