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

          errors ++= checkStatSemantics(
            paramTable.toMap,
            func.stats,
            Some(func.ty)
          )
        })

        // println("Stats: " + stats)

        errors ++= checkStatSemantics(Map.empty[Ident, Type], stats, None)
      }
    }

    errors.toList
  }

  def checkStatSemantics(
      symbolTable: Map[Ident, Type],
      stats: List[Stat],
      returnType: Option[Type]
  )(implicit
      funcTable: Map[Ident, (Type, List[Type])]
  ): List[String] = {
    val errors: mutable.ListBuffer[String] = mutable.ListBuffer.empty

    val scopedSymbolTable: mutable.Map[Ident, Type] = mutable.Map.empty

    stats.foreach(stat => {
      stat match {
        case Skip() => ()
        case Declare(ty, ident, rvalue) => {
          if (
            (symbolTable contains ident) || (scopedSymbolTable contains ident)
          )
            errors += "Redefined variable: " + ident.name
          else
            // TODO: add newly declared vars to symbol table
            scopedSymbolTable += (ident -> ty)
          val (rValueType, rValueErrors) = evalTypeOfRValue(rvalue, symbolTable)
          errors.addAll(rValueErrors)
          println("ty: " + ty)
          println("rValueType: " + rValueType)
          if (!(rValueType equiv ty))
            errors += f"Type mismatch: expected $ty, got $rValueType"
        }
        case Assign(lValue, rValue) => {
          val (lValueType, lValueErrors) = evalTypeOfLValue(lValue, symbolTable)
          val (rValueType, rValueErrors) = evalTypeOfRValue(rValue, symbolTable)
          errors ++= lValueErrors
          errors ++= rValueErrors

          if (!(lValueType equiv rValueType))
            errors += f"Type mismatch: expected $lValueType, got $rValueType"
        }
        case Read(lValue) => {
          val (lValueType, lValueErrors) = evalTypeOfLValue(lValue, symbolTable)
          errors ++= lValueErrors

          lValueType match {
            case IntType() | CharType() => ()
            case _ =>
              errors += f"Type mismatch: expected Int or Char, got $lValueType"
          }
        }

        case Free(expr) => {
          val (exprType, exprTypeErrors) = evalTypeOfExpr(expr, symbolTable)
          errors ++= exprTypeErrors

          exprType match {
            case PairType(_, _) | ArrayType(_) =>
            case _ =>
              errors += f"Type mismatch: expected Array or Pair, got $exprType"
          }
        }

        case Return(expr) => {
          val (exprType, exprTypeErrors) = evalTypeOfExpr(expr, symbolTable)
          errors ++= exprTypeErrors

          (returnType, exprType) match {
            case (Some(returnType), exprType) =>
              if (!(returnType equiv exprType))
                errors += f"Type mismatch: expected $returnType, got $exprType"
            case (None, _) => errors += "Return statement outside function"
          }
        }

        case Exit(expr) => {
          val (exprType, exprTypeErrors) = evalTypeOfExpr(expr, symbolTable)
          errors ++= exprTypeErrors
          exprType match {
            case IntType() =>
            case _ => errors += f"Type mismatch: expected Int, got $exprType"
          }
        }

        case Print(expr) => {
          val (exprType, exprTypeErrors) = evalTypeOfExpr(expr, symbolTable)
          errors ++= exprTypeErrors
        }

        case Println(expr) => {
          val (exprType, exprTypeErrors) = evalTypeOfExpr(expr, symbolTable)
          errors ++= exprTypeErrors
        }

        case If(cond, thenStat, elseStat) => {
          val (condType, condTypeErrors) = evalTypeOfExpr(cond, symbolTable)
          errors ++= condTypeErrors
          println("Condition type is: " + condType)

          condType match {
            // case IntType() => println("Hello")
            case BoolType() => ()
            case _ => errors += f"Type mismatch: expected Bool, got $condType"
          }

          errors ++= checkStatSemantics(
            symbolTable ++ scopedSymbolTable,
            thenStat,
            returnType
          )
          errors ++= checkStatSemantics(
            symbolTable ++ scopedSymbolTable,
            elseStat,
            returnType
          )

        }

        case While(cond, doStat) => {
          val (condType, condTypeErrors) = evalTypeOfExpr(cond, symbolTable)
          errors ++= condTypeErrors
          condType match {
            case BoolType() => ()
            case _ => errors += f"Type mismatch: expected Bool, got $condType"
          }
          errors ++= checkStatSemantics(
            symbolTable ++ scopedSymbolTable,
            doStat,
            returnType
          )
        }

        case Scope(scopeStats) => {
          errors ++= checkStatSemantics(
            symbolTable ++ scopedSymbolTable,
            scopeStats,
            returnType
          )
        }

        case _ => errors += "TODO: Implement checkStatSemantics"
      }
    })

    errors.toList
  }

  def evalTypeOfLValue(lValue: LValue, symbolTable: Map[Ident, Type])(implicit
      funcTable: Map[Ident, (Type, List[Type])]
  ): (Type, List[String]) = {
    val errors: mutable.ListBuffer[String] = mutable.ListBuffer.empty

    lValue match {
      case ident: Ident => {
        if (!symbolTable.contains(ident))
          (ErrorType, errors += f"Variable $ident not defined")

        (symbolTable(ident), errors.toList)
      }
      case ArrayElem(ident, xs) => {
        if (!symbolTable.contains(ident))
          (ErrorType, errors += f"Variable $ident not defined")

        (symbolTable(ident), errors.toList)
      }
      case Fst(l) => {
        val (lValueType, lValueErrors) = evalTypeOfLValue(l, symbolTable)
        errors ++= lValueErrors
        (lValueType, errors.toList)
      }
      case Snd(l) => {
        val (lValueType, lValueErrors) = evalTypeOfLValue(l, symbolTable)
        errors ++= lValueErrors
        (lValueType, errors.toList)
      }
    }
  }

  def evalTypeOfRValue(rValue: RValue, symbolTable: Map[Ident, Type])(implicit
      funcTable: Map[Ident, (Type, List[Type])]
  ): (Type, List[String]) = {
    val errors: mutable.ListBuffer[String] = mutable.ListBuffer.empty

    rValue match {
      case expr: Expr => evalTypeOfExpr(expr, symbolTable)
      case arrayLit @ ArrayLit(xs) => {
        xs match {
          case Nil =>
            (ArrayType(AnyType()(NULLPOS))(arrayLit.pos), errors.toList)
          case xs @ (head :: tail) => {
            val (expectedArrElemType, headErrors) =
              evalTypeOfExpr(head, symbolTable)
            errors ++= headErrors

            val (actualArrElemType, tailErrors) =
              checkExprs(tail, expectedArrElemType, symbolTable)
            errors ++= tailErrors
            (actualArrElemType, errors.toList)
          }
        }
      }
      case NewPair(fst, snd) => {
        val (fstType, fstErrors) = evalTypeOfExpr(fst, symbolTable)
        val (sndType, sndErrors) = evalTypeOfExpr(snd, symbolTable)
        errors ++= fstErrors
        errors ++= sndErrors
        (
          PairType(fstType.eraseInnerTypes, sndType.eraseInnerTypes)(NULLPOS),
          errors.toList
        )
      }
      case Call(f, args) => {
        if (!funcTable.contains(f))
          (ErrorType, errors += f"Function $f not defined")

        val (argTypes, argErrors) =
          args.map(evalTypeOfExpr(_, symbolTable)).unzip
        errors ++= argErrors.flatten

        val (returnType, paramTypes) = funcTable(f)
        if (argTypes.length != paramTypes.length)
          errors += f"Function $f called with ${argTypes.length} arguments, expected ${paramTypes.length}"

        argTypes.zip(paramTypes).foreach {
          case (argType, paramType) => {
            if (!argType.equals(paramType))
              errors += f"Function $f called with argument of type $argType, expected $paramType"
          }
        }

        (returnType, errors.toList)
      }
      case Fst(lValue) => {
        val (exprType, error) = evalTypeOfLValue(lValue, symbolTable)
        errors ++= error
        exprType match {
          case PairType(fstType, _) => (fstType.asType, errors.toList)
          case _                    => (ErrorType()(NULLPOS), errors.toList)
        }
      }
      case Snd(lValue) => {
        val (exprType, error) = evalTypeOfLValue(lValue, symbolTable)
        errors ++= error
        exprType match {
          case PairType(_, sndType) => (sndType.asType, errors.toList)
          case _                    => (ErrorType()(NULLPOS), errors.toList)
        }
      }
    }
  }

  def evalTypeOfExpr(
      expr: Expr,
      symbolTable: Map[Ident, Type]
  ): (Type, List[String]) = {
    expr match {
      case intLit: IntegerLiter => (IntType()(intLit.pos), Nil)
      case boolLit: BoolLiter   => (BoolType()(boolLit.pos), Nil)
      case charLit: CharLiter   => (CharType()(charLit.pos), Nil)
      case strLit: StrLiter     => (StringType()(strLit.pos), Nil)
      case nullPairLit @ Null() =>
        (
          PairType(AnyType()(nullPairLit.pos), AnyType()(nullPairLit.pos))(
            nullPairLit.pos
          ),
          Nil
        )
      case ident: Ident => {
        if (symbolTable contains ident) {
          (symbolTable(ident), Nil)
        } else {
          (ErrorType()(NULLPOS), List("Undeclared variable: " + ident.name))
        }
      }
      case ArrayElem(ident, xs) =>
        checkExprs(xs, IntType()(NULLPOS), symbolTable)
      case not: Not    => checkExprType(not, BoolType()(not.pos), symbolTable)
      case neg: Negate => checkExprType(neg, IntType()(neg.pos), symbolTable)
      case len: Len =>
        checkExprType(len, ArrayType(AnyType()(len.pos))(len.pos), symbolTable)
      case ord: Ord => checkExprType(ord, CharType()(ord.pos), symbolTable)
      case chr: Chr => checkExprType(chr, IntType()(chr.pos), symbolTable)
      case mul @ Mult(x, y) =>
        check2ExprType(x, y, IntType()(mul.pos), symbolTable)
      case div @ Div(x, y) =>
        check2ExprType(x, y, IntType()(div.pos), symbolTable)
      case mod @ Mod(x, y) =>
        check2ExprType(x, y, IntType()(mod.pos), symbolTable)
      case add @ Add(x, y) =>
        check2ExprType(x, y, IntType()(add.pos), symbolTable)
      case sub @ Sub(x, y) =>
        check2ExprType(x, y, IntType()(sub.pos), symbolTable)
      case eq @ Equal(x, y) =>
        check2ExprType(x, y, IntType()(eq.pos), symbolTable)
      case neq @ NotEqual(x, y) =>
        check2ExprType(x, y, IntType()(neq.pos), symbolTable)
      case lt @ LT(x, y) =>
        check2ExprType(x, y, IntType()(lt.pos), symbolTable)
      case lte @ LTE(x, y) =>
        check2ExprType(x, y, IntType()(lte.pos), symbolTable)
      case and @ And(x, y) =>
        check2ExprType(x, y, BoolType()(and.pos), symbolTable)
      case or @ Or(x, y) =>
        check2ExprType(x, y, BoolType()(or.pos), symbolTable)
      case gt @ GT(x, y) =>
        check2ExprType(x, y, IntType()(gt.pos), symbolTable)
      case gte @ GTE(x, y) =>
        check2ExprType(x, y, IntType()(gte.pos), symbolTable)
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

    if (exprType equiv expectedType) {
      (expectedType, error)
    } else {
      (
        ErrorType()(NULLPOS),
        error :+ f"Expected type $expectedType but got $exprType instead"
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
    if ((expr1Type equiv expectedType) && (expr2Type equiv expectedType)) {
      (expectedType, errors)
    } else {
      (
        ErrorType()(NULLPOS),
        errors :+ f"Expected type $expectedType but got $expr1Type and $expr2Type instead"
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
    if (types.distinct.length == 1 && (types.head equiv expectedType)) {
      (types.head, errors)
    } else {
      (
        (
          ErrorType()(NULLPOS),
          errors :+ f"Expected type $expectedType but got $types instead"
        )
      )
    }
  }

}
