package wacc

import parsley.Parsley
import parsley.position._

object genericbridgesPos {
  import parsley.implicits.zipped.{Zipped2, Zipped3, Zipped4}

  /** A bridge between a singleton and a parser
    * @tparam A
    *   The type of the singleton
    */
  trait ParserSingletonBridgePos[+A] {
    def con(pos: (Int, Int)): A
    def <#(op: Parsley[_]): Parsley[A] = pos.map(this.con(_)) <* op
  }

  /** A parser bridge pattern, with no arguments
    * @tparam R
    *   The type of the result
    */
  trait ParserBridgePos0[R] extends ParserSingletonBridgePos[R] {
    override final def con(pos: (Int, Int)): R = this.apply()(pos)

    def apply()(pos: (Int, Int)): R
  }

  /** A parser bridge pattern, with one argument
    * @tparam A
    *   The type of the argument
    * @tparam B
    *   The type of the result
    */
  trait ParserBridgePos1[-A, +B] extends ParserSingletonBridgePos[A => B] {
    override final def con(pos: (Int, Int)): A => B = this.apply(_)(pos)

    def apply(x: A)(pos: (Int, Int)): B

    def apply(x: Parsley[A]): Parsley[B] = pos <**> x.map(this.apply(_) _)
  }

  /** A parser bridge pattern, with two arguments
    * @tparam A
    *   The type of the first argument
    * @tparam B
    *   The type of the second argument
    * @tparam C
    *   The type of the result
    */
  trait ParserBridgePos2[-A, -B, +C]
      extends ParserSingletonBridgePos[(A, B) => C] {
    override final def con(pos: (Int, Int)): (A, B) => C = this.apply(_, _)(pos)

    def apply(x: A, y: B)(pos: (Int, Int)): C

    def apply(x: Parsley[A], y: => Parsley[B]): Parsley[C] =
      pos <**> (x, y).zipped(this.apply(_, _) _)
  }

  /** A parser bridge pattern, with three arguments
    * @tparam A
    *   The type of the first argument
    * @tparam B
    *   The type of the second argument
    * @tparam C
    *   The type of the third argument
    * @tparam D
    *   The type of the result
    */
  trait ParserBridgePos3[-A, -B, -C, +D]
      extends ParserSingletonBridgePos[(A, B, C) => D] {
    override final def con(pos: (Int, Int)): (A, B, C) => D =
      this.apply(_, _, _)(pos)

    def apply(x: A, y: B, z: C)(pos: (Int, Int)): D

    def apply(x: Parsley[A], y: => Parsley[B], z: => Parsley[C]): Parsley[D] =
      pos <**> (x, y, z).zipped(this.apply(_, _, _) _)
  }

  /** A parser bridge pattern, with four arguments
    * @tparam A
    *   The type of the first argument
    * @tparam B
    *   The type of the second argument
    * @tparam C
    *   The type of the third argument
    * @tparam D
    *   The type of the fourth argument
    * @tparam E
    *   The type of the result
    */
  trait ParserBridgePos4[-A, -B, -C, -D, +E]
      extends ParserSingletonBridgePos[(A, B, C, D) => E] {
    override final def con(pos: (Int, Int)): (A, B, C, D) => E =
      this.apply(_, _, _, _)(pos)

    def apply(x: A, y: B, z: C, w: D)(pos: (Int, Int)): E

    def apply(
        x: Parsley[A],
        y: => Parsley[B],
        z: => Parsley[C],
        w: => Parsley[D]
    ): Parsley[E] =
      pos <**> (x, y, z, w).zipped(this.apply(_, _, _, _) _)
  }
}

object AST {
  import genericbridgesPos._

  // Null position
  val NULLPOS: (Int, Int) = (-1, -1)

  // Statements
  sealed trait Stat {
    val pos: (Int, Int)
  }

  // LValues
  sealed trait LValue {
    val pos: (Int, Int)
  }

  sealed trait PairElem extends LValue with RValue

  // RValues
  sealed trait RValue {
    val pos: (Int, Int)
  }

  sealed trait Expr extends RValue {
    def size: Int = this match {
      // Needs to consider further cases
      case CharLiter(_) | BoolLiter(_) => 1
      case _                           => 4
    }
  }

// Types
  sealed trait WACCType {
    def equiv(that: WACCType): Boolean =
      (this, that) match {
        case (_, AnyType()) | (AnyType(), _)         => true
        case (_, UnknownType()) | (UnknownType(), _) => true
        case (_, ErrorType()) | (ErrorType(), _)     => false
        case (ArrayType(thisTy), ArrayType(thatTy))  => thisTy equiv thatTy
        case (ArrayType(CharType()), StringType())   => true
        case (
              PairType(thisFstType, thisSndType),
              PairType(thatFstType, thatSndType)
            ) =>
          (thisFstType equiv thatFstType) && (thisSndType equiv thatSndType)
        case _ => this == that
      }
  }

  sealed trait Type extends WACCType {
    def positioned(pos: (Int, Int)): Type
    def size: Int = this match {
      case CharType() | BoolType() => 1
      case _                       => 4
    }
    def eraseInnerTypes: PairElemType

  }

  sealed trait PairElemType extends WACCType {
    def asType: Type
  }

  sealed trait GenericType extends Type with PairElemType {
    def asType: Type = this
    def eraseInnerTypes: PairElemType = this
  }

  // Base Types
  sealed trait BaseType extends GenericType

  sealed trait PairLiter extends Expr

  /* Case Classes and Traits */
  case class Program(funcs: List[Func], stat: List[Stat])(val pos: (Int, Int)) {
    var symbolTable: Map[Ident, Type] = Map.empty
    var printTable: Map[(Int, Int), Type] = Map.empty
  }

  case class Func(
      ty: Type,
      ident: Ident,
      paramList: List[Param],
      stats: List[Stat]
  )(
      val pos: (Int, Int)
  ) {
    var symbolTable: Map[Ident, Type] = Map.empty
    var printTable: Map[(Int, Int), Type] = Map.empty
  }

  case class Param(ty: Type, ident: Ident)(val pos: (Int, Int))

  case class Skip()(val pos: (Int, Int)) extends Stat

  case class Assign(lValue: LValue, y: RValue)(val pos: (Int, Int)) extends Stat

  case class Declare(ty: Type, x: Ident, y: RValue)(val pos: (Int, Int))
      extends Stat

  case class Read(lValue: LValue)(val pos: (Int, Int)) extends Stat

  case class Free(expr: Expr)(val pos: (Int, Int)) extends Stat

  case class Return(expr: Expr)(val pos: (Int, Int)) extends Stat

  case class Exit(expr: Expr)(val pos: (Int, Int)) extends Stat

  case class Print(expr: Expr)(val pos: (Int, Int)) extends Stat

  case class Println(expr: Expr)(val pos: (Int, Int)) extends Stat

  case class If(cond: Expr, thenStat: List[Stat], elseStat: List[Stat])(
      val pos: (Int, Int)
  ) extends Stat {
    var thenSymbolTable: Map[Ident, Type] = Map.empty
    var elseSymbolTable: Map[Ident, Type] = Map.empty
  }

  case class While(cond: Expr, doStat: List[Stat])(val pos: (Int, Int))
      extends Stat {
    var symbolTable: Map[Ident, Type] = Map.empty
  }

  case class Scope(stats: List[Stat])(val pos: (Int, Int)) extends Stat {
    var symbolTable: Map[Ident, Type] = Map.empty
  }

  case class Ident(name: String)(val pos: (Int, Int)) extends LValue with Expr {
    override def toString(): String = s"identifier ${name}"
  }

  case class ArrayElem(ident: Ident, xs: List[Expr])(val pos: (Int, Int))
      extends LValue
      with Expr

  case class Fst(p: LValue)(val pos: (Int, Int)) extends PairElem

  case class Snd(p: LValue)(val pos: (Int, Int)) extends PairElem

  case class ArrayLit(xs: List[Expr])(val pos: (Int, Int)) extends RValue

  case class NewPair(fst: Expr, snd: Expr)(val pos: (Int, Int)) extends RValue

  case class Call(x: Ident, args: List[Expr])(val pos: (Int, Int))
      extends RValue

  case class AnyType()(val pos: (Int, Int)) extends GenericType {
    def positioned(pos: (Int, Int)): AnyType = AnyType()(pos)
    override def toString(): String = "any"
  }

  case class ErrorType()(val pos: (Int, Int)) extends GenericType {
    def positioned(pos: (Int, Int)): ErrorType = ErrorType()(pos)
    override def toString(): String = "ERROR"
  }

  case class UnknownType()(val pos: (Int, Int)) extends GenericType {
    def positioned(pos: (Int, Int)): UnknownType = UnknownType()(pos)
    override def toString(): String = "?"
  }

  case class IntType()(val pos: (Int, Int)) extends BaseType {
    def positioned(pos: (Int, Int)): IntType = IntType()(pos)
    override def toString(): String = "int"
  }

  case class BoolType()(val pos: (Int, Int)) extends BaseType {
    def positioned(pos: (Int, Int)): BoolType = BoolType()(pos)
    override def toString(): String = "bool"
  }

  case class CharType()(val pos: (Int, Int)) extends BaseType {
    def positioned(pos: (Int, Int)): CharType = CharType()(pos)
    override def toString(): String = "char"
  }

  case class StringType()(val pos: (Int, Int)) extends BaseType {
    def positioned(pos: (Int, Int)): StringType = StringType()(pos)
    override def toString(): String = "string"
  }

  // Array Types
  case class ArrayType(ty: Type)(val pos: (Int, Int)) extends GenericType {
    def positioned(pos: (Int, Int)): ArrayType = ArrayType(ty)(pos)
    override def toString(): String = s"$ty[]"
  }

  // Pair Types
  case class PairType(fstType: PairElemType, sndType: PairElemType)(
      val pos: (Int, Int)
  ) extends Type {
    def eraseInnerTypes: PairElemType = InnerPairType()(pos)
    def positioned(pos: (Int, Int)): PairType = PairType(fstType, sndType)(pos)
    override def toString(): String = if (
      (fstType equiv UnknownType()(NULLPOS)) && (sndType equiv UnknownType()(
        NULLPOS
      ))
    ) "pair"
    else s"pair($fstType, $sndType)"
  }

  case class InnerPairType()(val pos: (Int, Int)) extends PairElemType {
    def asType: Type =
      PairType(UnknownType()(NULLPOS), UnknownType()(NULLPOS))(pos)
    def positioned(pos: (Int, Int)): InnerPairType = InnerPairType()(pos)
    override def toString(): String = "pair"
  }

  // Literals
  case class IntegerLiter(x: Int)(val pos: (Int, Int)) extends Expr

  case class BoolLiter(x: Boolean)(val pos: (Int, Int)) extends Expr

  case class CharLiter(x: Char)(val pos: (Int, Int)) extends Expr

  case class StrLiter(x: String)(val pos: (Int, Int)) extends Expr

  case class Null()(val pos: (Int, Int)) extends PairLiter

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
  case class Not(x: Expr)(val pos: (Int, Int)) extends Expr
  case class Neg(x: Expr)(val pos: (Int, Int)) extends Expr
  case class Len(x: Expr)(val pos: (Int, Int)) extends Expr
  case class Ord(x: Expr)(val pos: (Int, Int)) extends Expr
  case class Chr(x: Expr)(val pos: (Int, Int)) extends Expr

  case class Bracket(x: Expr)(val pos: (Int, Int)) extends Expr

  /* Companion Objects */
  object Program extends ParserBridgePos2[List[Func], List[Stat], Program]
  object Func
      extends ParserBridgePos4[Type, Ident, List[Param], List[Stat], Func]
  object Param extends ParserBridgePos2[Type, Ident, Param]

  // Statements
  object Skip extends ParserBridgePos0[Skip]
  object Assign extends ParserBridgePos2[LValue, RValue, Assign]
  object Declare extends ParserBridgePos3[Type, Ident, RValue, Declare]
  object Read extends ParserBridgePos1[LValue, Read]
  object Free extends ParserBridgePos1[Expr, Free]
  object Return extends ParserBridgePos1[Expr, Return]
  object Exit extends ParserBridgePos1[Expr, Exit]
  object Print extends ParserBridgePos1[Expr, Print]
  object Println extends ParserBridgePos1[Expr, Println]
  object If extends ParserBridgePos3[Expr, List[Stat], List[Stat], If]
  object While extends ParserBridgePos2[Expr, List[Stat], While]
  object Scope extends ParserBridgePos1[List[Stat], Scope]

  // LValues
  object Ident extends ParserBridgePos1[String, Ident]
  object ArrayElem extends ParserBridgePos2[Ident, List[Expr], ArrayElem]
  object Fst extends ParserBridgePos1[LValue, Fst]
  object Snd extends ParserBridgePos1[LValue, Snd]

  // RValues
  object ArrayLit extends ParserBridgePos1[List[Expr], ArrayLit]
  object NewPair extends ParserBridgePos2[Expr, Expr, NewPair]
  object Call extends ParserBridgePos2[Ident, List[Expr], Call]

  // Types
  object AnyType extends ParserBridgePos0[AnyType]
  object ErrorType extends ParserBridgePos0[ErrorType]
  object UnknownType extends ParserBridgePos0[UnknownType]
  object ArrayType extends ParserBridgePos1[Type, ArrayType]
  object IntType extends ParserBridgePos0[IntType]
  object BoolType extends ParserBridgePos0[BoolType]
  object CharType extends ParserBridgePos0[CharType]
  object StringType extends ParserBridgePos0[StringType]
  object PairType extends ParserBridgePos2[PairElemType, PairElemType, PairType]
  object InnerPairType extends ParserBridgePos0[InnerPairType]

  // Literals
  object IntegerLiter extends ParserBridgePos1[Int, IntegerLiter]
  object BoolLiter extends ParserBridgePos1[Boolean, BoolLiter]
  object CharLiter extends ParserBridgePos1[Char, CharLiter]
  object StrLiter extends ParserBridgePos1[String, StrLiter]
  object Null extends ParserBridgePos0[Null]

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
  object Not extends ParserBridgePos1[Expr, Expr]
  object Neg extends ParserBridgePos1[Expr, Expr]
  object Len extends ParserBridgePos1[Expr, Expr]
  object Ord extends ParserBridgePos1[Expr, Expr]
  object Chr extends ParserBridgePos1[Expr, Expr]

  object Bracket extends ParserBridgePos1[Expr, Expr]

}
