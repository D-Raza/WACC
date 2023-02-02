package wacc

import parsley.Parsley
import parsley.position._

object genericbridgesPos {
  import parsley.implicits.zipped.{Zipped2, Zipped3, Zipped4}

  trait ParserSingletonBridgePos[+A] {
    def con(pos: (Int, Int)): A
    def <#(op: Parsley[_]): Parsley[A] = pos.map(this.con(_)) <* op
  }

  trait ParserBridgePos0[R] extends ParserSingletonBridgePos[R] {
    override final def con(pos: (Int, Int)): R = this.asInstanceOf[R]
  }

  trait ParserBridgePos1[-A, +B] extends ParserSingletonBridgePos[A => B] {
    def apply(x: A)(pos: (Int, Int)): B
    def apply(x: Parsley[A]): Parsley[B] = pos <**> x.map(this.apply(_) _)
    override final def con(pos: (Int, Int)): A => B = this.apply(_)(pos)
  }

  trait ParserBridgePos2[-A, -B, +C]
      extends ParserSingletonBridgePos[(A, B) => C] {
    def apply(x: A, y: B)(pos: (Int, Int)): C
    def apply(x: Parsley[A], y: => Parsley[B]): Parsley[C] =
      pos <**> (x, y).zipped(this.apply(_, _) _)
    override final def con(pos: (Int, Int)): (A, B) => C = this.apply(_, _)(pos)
  }

  trait ParserBridgePos3[-A, -B, -C, +D]
      extends ParserSingletonBridgePos[(A, B, C) => D] {
    def apply(x: A, y: B, z: C)(pos: (Int, Int)): D
    def apply(x: Parsley[A], y: => Parsley[B], z: => Parsley[C]): Parsley[D] =
      pos <**> (x, y, z).zipped(this.apply(_, _, _) _)
    override final def con(pos: (Int, Int)): (A, B, C) => D =
      this.apply(_, _, _)(pos)
  }

  trait ParserBridgePos4[-A, -B, -C, -D, +E]
      extends ParserSingletonBridgePos[(A, B, C, D) => E] {
    def apply(x: A, y: B, z: C, w: D)(pos: (Int, Int)): E
    def apply(
        x: Parsley[A],
        y: => Parsley[B],
        z: => Parsley[C],
        w: => Parsley[D]
    ): Parsley[E] =
      pos <**> (x, y, z, w).zipped(this.apply(_, _, _, _) _)
    override final def con(pos: (Int, Int)): (A, B, C, D) => E =
      this.apply(_, _, _, _)(pos)
  }
}

object AST {
  import genericbridgesPos._

  /* Case Classes and Traits */
  case class Program(funcs: List[Func], stat: Stat)(val pos: (Int, Int))
  case class Func(ty: Type, ident: Ident, paramList: ParamList, stat: Stat)(
      val pos: (Int, Int)
  )
  case class ParamList(params: List[Param])(val pos: (Int, Int))
  case class Param(ty: Type, ident: Ident)(val pos: (Int, Int))

  // Statements
  sealed trait Stat
  case class Skip()(val pos: (Int, Int))
  case class Read(lValue: LValue)(val pos: (Int, Int))
  case class Free(expr: Expr)(val pos: (Int, Int))
  case class Return(expr: Expr)(val pos: (Int, Int))
  case class Exit(expr: Expr)(val pos: (Int, Int))
  case class Print(expr: Expr)(val pos: (Int, Int))
  case class Println(expr: Expr)(val pos: (Int, Int))
  case class IfStat(cond: Expr, thenStat: Stat, elseStat: Stat)(
      val pos: (Int, Int)
  )
  case class WhileStat(cond: Expr, doStat: Stat)(val pos: (Int, Int))
  case class Begin(beginStat: Stat)(val pos: (Int, Int))
  case class Stats(stat1: Stat, stat2: Stat)(val pos: (Int, Int))
  case class Assign(lValue: LValue, y: RValue)(val pos: (Int, Int))
  case class Declaration(ty: Type, x: Ident, y: RValue)(val pos: (Int, Int))

  // LValues
  sealed trait LValue
  sealed trait Expr extends LValue
  case class Ident(x: String)(val pos: (Int, Int)) extends LValue with Expr
  case class ArrayElem(ident: Ident, xs: List[Expr])(val pos: (Int, Int))
      extends LValue
      with Expr
  sealed trait PairElem extends LValue with RValue
  case class PairFst(lValue: LValue)(val pos: (Int, Int))
  case class PairSnd(lValue: LValue)(val pos: (Int, Int))

  // RValues
  sealed trait RValue
  case class ArrayLit(xs: List[Expr])(val pos: (Int, Int)) extends RValue
  case class FunctionCall(ident: Ident, args: List[ArgList])(
      val pos: (Int, Int)
  )
  case class NewPair(fst: Expr, snd: Expr)(val pos: (Int, Int))
  case class ArgList(args: List[Expr])(val pos: (Int, Int))
  case class Call(x: Ident, args: ArgList)(val pos: (Int, Int))

  // Types
  sealed trait Type
  sealed trait PairElemType
  sealed trait BaseType extends Type with PairElemType
  case class Pair()(val pos: (Int, Int)) extends PairElemType
  case class ArrayType(ty: Type)(val pos: (Int, Int))
      extends Type
      with PairElemType
  case class IntType()(val pos: (Int, Int)) extends BaseType
  case class BoolType()(val pos: (Int, Int)) extends BaseType
  case class CharType()(val pos: (Int, Int)) extends BaseType
  case class StringType()(val pos: (Int, Int)) extends BaseType
  case class PairType(fstType: PairElemType, sndType: PairElemType)(
      val pos: (Int, Int)
  ) extends Type
  case class Fst(x: Expr)(val pos: (Int, Int)) extends PairElem
  case class Snd(x: Expr)(val pos: (Int, Int)) extends PairElem

  // Literals
  case class IntegerLiter(x: BigInt)(val pos: (Int, Int)) extends Expr
  case class BoolLiter(x: Boolean)(val pos: (Int, Int)) extends Expr
  case class CharLiter(x: Char)(val pos: (Int, Int)) extends Expr
  case class StrLiter(x: String)(val pos: (Int, Int)) extends Expr
  case class PairLiter()(val pos: (Int, Int)) extends Expr

  // Binary operators
  case class Mult(x: Expr, y: Expr)(val pos: (Int, Int)) extends Expr
  case class Div(x: Expr, y: Expr)(val pos: (Int, Int)) extends Expr
  case class Mod(x: Expr, y: Expr)(val pos: (Int, Int)) extends Expr
  case class Add(x: Expr, y: Expr)(val pos: (Int, Int)) extends Expr
  case class Sub(x: Expr, y: Expr)(val pos: (Int, Int)) extends Expr
  case class Equal(x: Expr, y: Expr)(val pos: (Int, Int)) extends Expr
  case class NotEqual(x: Expr, y: Expr)(val pos: (Int, Int)) extends Expr
  case class LT(x: Expr, y: Expr)(val pos: (Int, Int)) extends Expr
  case class LTE(x: Expr, y: Expr)(val pos: (Int, Int)) extends Expr
  case class And(x: Expr, y: Expr)(val pos: (Int, Int)) extends Expr
  case class Or(x: Expr, y: Expr)(val pos: (Int, Int)) extends Expr
  case class GT(x: Expr, y: Expr)(val pos: (Int, Int)) extends Expr
  case class GTE(x: Expr, y: Expr)(val pos: (Int, Int)) extends Expr

  // Unary operators
  case class Not(x: Boolean)(val pos: (Int, Int)) extends Expr
  case class Negate(x: Int)(val pos: (Int, Int)) extends Expr
  case class Len(x: String)(val pos: (Int, Int)) extends Expr
  case class Ord(x: Char)(val pos: (Int, Int)) extends Expr
  case class Chr(x: Char)(val pos: (Int, Int)) extends Expr

  /* Companion Objects */
  object Program extends ParserBridgePos2[List[Func], Stat, Program]
  object Func extends ParserBridgePos4[Type, Ident, ParamList, Stat, Func]
  object ParamList extends ParserBridgePos1[List[Param], ParamList]
  object Param extends ParserBridgePos2[Type, Ident, Param]

  // Statements
  object Skip extends ParserBridgePos0[Skip]
  object Read extends ParserBridgePos1[LValue, Read]
  object Free extends ParserBridgePos1[Expr, Free]
  object Return extends ParserBridgePos1[Expr, Return]
  object Exit extends ParserBridgePos1[Expr, Exit]
  object Print extends ParserBridgePos1[Expr, Print]
  object Println extends ParserBridgePos1[Expr, Println]
  object IfStat extends ParserBridgePos3[Expr, Stat, Stat, IfStat]
  object WhileStat extends ParserBridgePos2[Expr, Stat, WhileStat]
  object Begin extends ParserBridgePos1[Stat, Begin]
  object Stats extends ParserBridgePos2[Stat, Stat, Stats]
  object Assign extends ParserBridgePos2[LValue, RValue, Assign]
  object Declaration extends ParserBridgePos3[Type, Ident, RValue, Declaration]

  // LValues
  object Ident extends ParserBridgePos1[String, Ident]
  object ArrayElem extends ParserBridgePos2[Ident, List[Expr], ArrayElem]
  object PairFst extends ParserBridgePos1[LValue, PairFst]
  object PairSnd extends ParserBridgePos1[LValue, PairSnd]

  // RValues
  object ArrayLit extends ParserBridgePos1[List[Expr], ArrayLit]
  object FunctionCall
      extends ParserBridgePos2[Ident, List[ArgList], FunctionCall]
  object NewPair extends ParserBridgePos2[Expr, Expr, NewPair]
  object ArgList extends ParserBridgePos1[List[Expr], ArgList]
  object Call extends ParserBridgePos2[Ident, ArgList, Call]

  // Types
  object Pair extends ParserBridgePos0[Pair]
  object ArrayType extends ParserBridgePos1[Type, ArrayType]
  object IntType extends ParserBridgePos0[IntType]
  object BoolType extends ParserBridgePos0[BoolType]
  object CharType extends ParserBridgePos0[CharType]
  object StringType extends ParserBridgePos0[StringType]
  object PairType extends ParserBridgePos2[PairElemType, PairElemType, PairType]
  object Fst extends ParserBridgePos1[Expr, Fst]
  object Snd extends ParserBridgePos1[Expr, Snd]

  // Literals
  object IntegerLiter extends ParserBridgePos1[BigInt, IntegerLiter]
  object BoolLiter extends ParserBridgePos1[Boolean, BoolLiter]
  object CharLiter extends ParserBridgePos1[Char, CharLiter]
  object StrLiter extends ParserBridgePos1[String, StrLiter]
  object PairLiter extends ParserBridgePos0[PairLiter]

  // Binary operators
  object Mult extends ParserBridgePos2[Expr, Expr, Mult]
  object Div extends ParserBridgePos2[Expr, Expr, Div]
  object Mod extends ParserBridgePos2[Expr, Expr, Mod]
  object Add extends ParserBridgePos2[Expr, Expr, Add]
  object Sub extends ParserBridgePos2[Expr, Expr, Sub]
  object Equal extends ParserBridgePos2[Expr, Expr, Equal]
  object NotEqual extends ParserBridgePos2[Expr, Expr, NotEqual]
  object LT extends ParserBridgePos2[Expr, Expr, LT]
  object LTE extends ParserBridgePos2[Expr, Expr, LTE]
  object And extends ParserBridgePos2[Expr, Expr, And]
  object Or extends ParserBridgePos2[Expr, Expr, Or]
  object GT extends ParserBridgePos2[Expr, Expr, GT]
  object GTE extends ParserBridgePos2[Expr, Expr, GTE]

  // Unary operators
  object Not extends ParserBridgePos1[Boolean, Not]
  object Negate extends ParserBridgePos1[Int, Negate]
  object Len extends ParserBridgePos1[String, Len]
  object Ord extends ParserBridgePos1[Char, Ord]
  object Chr extends ParserBridgePos1[Char, Chr]

}
