package wacc.frontend

import wacc.AST._
import scala.collection.mutable

object SemanticAnalyser {

  def checkProgramSemantics(program: Program): List[String] = {
    val errors: mutable.ListBuffer[String] = mutable.ListBuffer.empty
    val functionDefs: mutable.Map[Ident, (Type, List[Type])] = mutable.Map.empty

    program match {
      case Program(funcs, stats) => {
        funcs.foreach(func => {
          if (functionDefs contains func.ident)
            errors += "Redefined function: " + func.ident.name

          functionDefs += (func.ident -> (func.ty, func.paramList.map(_.ty)))
        })
        implicit val funcTable: Map[Ident, (Type, List[Type])] =
          functionDefs.toMap

        funcs.foreach(func => {
          val paramTable: mutable.Map[Ident, Type] = mutable.Map.empty

          func.paramList.foreach(param => {
            if (paramTable contains param.ident)
              errors += "Redefined parameter: " + param.ident.name
            paramTable += (param.ident -> param.ty)
          })

          errors ++= checkStatSemantics(paramTable.toMap, func.stats)
        })

        errors ++= checkStatSemantics(Map.empty[Ident, Type], stats)
      }
    }

    errors.toList
  }

  def checkStatSemantics(symbolTable: Map[Ident, Type], stats: List[Stat])(
      implicit funcTable: Map[Ident, (Type, List[Type])]
  ): List[String] = {
    val errors: mutable.ListBuffer[String] = mutable.ListBuffer.empty

    // stats.foreach(stat => {
    //   stat match {
    //     case Skip() => ()
    //     case Declare(ty, ident, rvalue) => {
    //       if (symbolTable contains ident)
    //         errors += "Redefined variable: " + ident.name

    //       val (rValueType, rValueErrors) = evalTypeOfRValue(rvalue, symbolTable)
    //       errors.addAll(rValueErrors)
    //       if(!rValueType.equals(ty)) {

    //       }

    //     }
    //     case Assign(lValue, y) => {

    //     }
    //     case _ => errors += "TODO: Implement checkStatSemantics"
    //   }
    // })

    errors.toList
  }

  // def evalTypeOfRValue(rValue: RValue, symbolTable: Map[Ident, Type])(implicit funcTable: Map[Ident, (Type, List[Type])]) : (Type, List[String]) = {
  //   val errors : mutable.ListBuffer[String] = mutable.ListBuffer.empty

  //   rValue match {
  //     case expr: Expr => {
  //       val (exprType, exprErrors) = evalTypeOfExpr(expr, symbolTable)
  //       (exprType, errors.toList)
  //     }
  //     case NewPair(fst, snd) => {
  //       val (fstType, fstErrors) = evalTypeOfExpr(fst, symbolTable)
  //       val (sndType, sndErrors) = evalTypeOfExpr(snd, symbolTable)
  //       errors ++= fstErrors
  //       errors ++= sndErrors
  //       (PairType(fstType, sndType)(NULLPOS), errors.toList)
  //     }
  //     case Call(x, args) => {

  //     }
  //     case Fst(p) => {

  //     }
  //     case Snd(p) => {

  //     }
  //     case _ => ()
  //   }
  // }

  def evalTypeOfExpr(
      expr: Expr,
      symbolTable: Map[Ident, Type]
  ): (Type, List[String]) = {
    expr match {
      case ident: Ident => {
        if (symbolTable contains ident) {
          (symbolTable(ident), Nil)
        } else {
          (ErrorType()(NULLPOS), List("Undeclared variable: " + ident.name))
        }
      }
      case ArrayElem(ident, xs) =>
        checkExprs(xs, IntType()(NULLPOS), symbolTable)
      case intLit: IntegerLiter => (IntType()(NULLPOS), Nil)
      case boolLit: BoolLiter   => (BoolType()(NULLPOS), Nil)
      case charLit: CharLiter   => (CharType()(NULLPOS), Nil)
      case strLit: StrLiter     => (StringType()(NULLPOS), Nil)
      case pairLit: PairLiter   => (NullType()(NULLPOS), Nil)
      case mul @ Mult(x, y) =>
        check2ExprType(x, y, IntType()(NULLPOS), symbolTable)
      case div @ Div(x, y) =>
        check2ExprType(x, y, IntType()(NULLPOS), symbolTable)
      case mod @ Mod(x, y) =>
        check2ExprType(x, y, IntType()(NULLPOS), symbolTable)
      case add @ Add(x, y) =>
        check2ExprType(x, y, IntType()(NULLPOS), symbolTable)
      case sub @ Sub(x, y) =>
        check2ExprType(x, y, IntType()(NULLPOS), symbolTable)
      case eq @ Equal(x, y) =>
        check2ExprType(x, y, IntType()(NULLPOS), symbolTable)
      case neq @ NotEqual(x, y) =>
        check2ExprType(x, y, IntType()(NULLPOS), symbolTable)
      case lt @ LT(x, y) =>
        check2ExprType(x, y, IntType()(NULLPOS), symbolTable)
      case lte @ LTE(x, y) =>
        check2ExprType(x, y, IntType()(NULLPOS), symbolTable)
      case and @ And(x, y) =>
        check2ExprType(x, y, BoolType()(NULLPOS), symbolTable)
      case or @ Or(x, y) =>
        check2ExprType(x, y, BoolType()(NULLPOS), symbolTable)
      case gt @ GT(x, y) =>
        check2ExprType(x, y, IntType()(NULLPOS), symbolTable)
      case gte @ GTE(x, y) =>
        check2ExprType(x, y, IntType()(NULLPOS), symbolTable)
      case not: Not    => checkExprType(not, BoolType()(NULLPOS), symbolTable)
      case neg: Negate => checkExprType(neg, IntType()(NULLPOS), symbolTable)
      case len: Len =>
        (
          NullType()(NULLPOS),
          Nil
        ) // checkExprType(len, ArrayType()(NULLPOS), symbolTable)
      case ord: Ord   => checkExprType(ord, CharType()(NULLPOS), symbolTable)
      case chr: Chr   => checkExprType(chr, IntType()(NULLPOS), symbolTable)
      case Bracket(x) => evalTypeOfExpr(x, symbolTable)
    }
  }

  // Check that an expression is of a certain type
  private def checkExprType(
      expr: Expr,
      expectedType: Type,
      symbolTable: Map[Ident, Type]
  ): (Type, List[String]) = {
    val (exprType, error) = evalTypeOfExpr(expr, symbolTable)
    exprType match {
      case exprType if exprType == expectedType => (exprType, error)
      case _ =>
        (
          ErrorType()(NULLPOS),
          error :+ "Expected type " + expectedType + " but got " + exprType + " instead"
        )
    }
  }

  // Check that two expressions are of the same expected type
  private def check2ExprType(
      expr1: Expr,
      expr2: Expr,
      expectedType: Type,
      symbolTable: Map[Ident, Type]
  ): (Type, List[String]) = {
    val (expr1Type, error1) = evalTypeOfExpr(expr1, symbolTable)
    val (expr2Type, error2) = evalTypeOfExpr(expr2, symbolTable)
    val errors = error1 ++ error2
    expr1Type match {
      case expr1Type if expr1Type == expectedType && expr1Type == expr2Type =>
        (expr1Type, errors)
      case _ =>
        (
          ErrorType()(NULLPOS),
          errors :+ "Expected type " + expectedType + " but got " + expr1Type + " instead"
        )
    }
  }

  // Check that a list of expressions are of a certain type
  private def checkExprs(
      exprs: List[Expr],
      expectedType: Type,
      symbolTable: Map[Ident, Type]
  ): (Type, List[String]) = {
    val evals = exprs.map(checkExprType(_, expectedType, symbolTable))
    val types = evals.map(_._1)
    val errors = evals.flatMap(_._2)
    if (types.distinct.length == 1 && types.head == expectedType) {
      (types.head, errors)
    } else {
      (
        ErrorType()(NULLPOS),
        errors :+ "Expected type " + expectedType + " but got " + types + " instead"
      )
    }
  }

}
