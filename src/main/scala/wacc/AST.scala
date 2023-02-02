package wacc

object AST {
  case class Program(funcs: List[Func], stat: Stat)(pos: (Int, Int))
  case class Func(ty: Type, ident: Ident, paramList: ParamList, stat: Stat)(
      pos: (Int, Int)
  )
  case class ParamList(params: List[Param])(pos: (Int, Int))
  case class Param(ty: Type, ident: Ident)(pos: (Int, Int))

  // Statements
  sealed trait Stat
  case class Skip()(pos: (Int, Int))
  case class Read(lValue: LValue)(pos: (Int, Int))
  case class Free(expr: Expr)(pos: (Int, Int))
  case class Return(expr: Expr)(pos: (Int, Int))
  case class Exit(expr: Expr)(pos: (Int, Int))
  case class Print(expr: Expr)(pos: (Int, Int))
  case class Println(expr: Expr)(pos: (Int, Int))
  case class IfStat(cond: Expr, thenStat: Stat, elseStat: Stat)(pos: (Int, Int))
  case class WhileStat(cond: Expr, doStat: Stat)(pos: (Int, Int))
  case class Begin(beginStat: Stat)(pos: (Int, Int))
  case class Stats(stat1: Stat, stat2: Stat)(pos: (Int, Int))
  case class Assign(lValue: LValue, y: RValue)(pos: (Int, Int))
  case class Declaration(ty: Type, x: Ident, y: RValue)(pos: (Int, Int))

  sealed trait LValue
  sealed trait Expr extends LValue
  case class Ident(x: String)(pos: (Int, Int)) extends LValue with Expr
  case class ArrayElem(ident: Ident, xs: List[Expr])(pos: (Int, Int))
      extends LValue
      with Expr
  sealed trait PairElem extends LValue with RValue
  case class PairFst(lValue: LValue)(pos: (Int, Int))
  case class PairSnd(lValue: LValue)(pos: (Int, Int))

  sealed trait RValue
  case class ArrayLit(xs: List[Expr])(pos: (Int, Int)) extends RValue
  case class FunctionCall(ident: Ident, args: List[ArgList])(pos: (Int, Int))
  case class NewPair(fst: Expr, snd: Expr)(pos: (Int, Int))
  case class ArgList(args: List[Expr])(pos: (Int, Int))
  case class Call(x: Ident, args: ArgList)(pos: (Int, Int))

  // Types
  sealed trait Type
  sealed trait PairElemType
  sealed trait BaseType extends Type with PairElemType
  case class Pair()(pos: (Int, Int)) extends PairElemType
  case class ArrayType(ty: Type)(pos: (Int, Int)) extends Type with PairElemType
  case class IntType()(pos: (Int, Int)) extends BaseType
  case class BoolType()(pos: (Int, Int)) extends BaseType
  case class CharType()(pos: (Int, Int)) extends BaseType
  case class StringType()(pos: (Int, Int)) extends BaseType
  case class PairType(fstType: PairElemType, sndType: PairElemType)(
      pos: (Int, Int)
  ) extends Type
  case class Fst(x: Expr)(pos: (Int, Int)) extends PairElem
  case class Snd(x: Expr)(pos: (Int, Int)) extends PairElem

  // Literals
  case class IntegerLiter(x: BigInt)(pos: (Int, Int)) extends Expr
  case class BoolLiter(x: Boolean)(pos: (Int, Int)) extends Expr
  case class CharLiter(x: Char)(pos: (Int, Int)) extends Expr
  case class StrLiter(x: String)(pos: (Int, Int)) extends Expr
  case class PairLiter()(pos: (Int, Int)) extends Expr

  // Binary operators
  case class Mult(x: Expr, y: Expr)(pos: (Int, Int)) extends Expr
  case class Div(x: Expr, y: Expr)(pos: (Int, Int)) extends Expr
  case class Mod(x: Expr, y: Expr)(pos: (Int, Int)) extends Expr
  case class Add(x: Expr, y: Expr)(pos: (Int, Int)) extends Expr
  case class Sub(x: Expr, y: Expr)(pos: (Int, Int)) extends Expr
  case class Equal(x: Expr, y: Expr)(pos: (Int, Int)) extends Expr
  case class NotEqual(x: Expr, y: Expr)(pos: (Int, Int)) extends Expr
  case class LT(x: Expr, y: Expr)(pos: (Int, Int)) extends Expr
  case class LTE(x: Expr, y: Expr)(pos: (Int, Int)) extends Expr
  case class And(x: Expr, y: Expr)(pos: (Int, Int)) extends Expr
  case class Or(x: Expr, y: Expr)(pos: (Int, Int)) extends Expr
  case class GT(x: Expr, y: Expr)(pos: (Int, Int)) extends Expr
  case class GTE(x: Expr, y: Expr)(pos: (Int, Int)) extends Expr

  // Unary operators
  case class Not(x: Boolean)(pos: (Int, Int)) extends Expr
  case class Negate(x: Int)(pos: (Int, Int)) extends Expr
  case class Len(x: String)(pos: (Int, Int)) extends Expr
  case class Ord(x: Char)(pos: (Int, Int)) extends Expr
  case class Chr(x: Char)(pos: (Int, Int)) extends Expr
}
