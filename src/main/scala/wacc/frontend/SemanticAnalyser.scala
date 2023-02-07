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
    implicit var curSymbolTable: Map[Ident, Type] =
      symbolTable ++ scopedSymbolTable.toMap

    stats.foreach(stat => {
      stat match {
        case Skip() => ()

        case d @ Declare(ty, ident, rvalue) => {
          if (scopedSymbolTable contains ident)
            errors += "Redefined variable: " + ident.name
          else {
            scopedSymbolTable += (ident -> ty)
            curSymbolTable = symbolTable ++ scopedSymbolTable.toMap
          }

          val (rValueType, rValueErrors) = evalTypeOfRValue(rvalue)
          errors ++= rValueErrors
          if (!(rValueType equiv ty))
            errors += f"Type mismatch: expected $ty, got $rValueType"
        }

        case Assign(lValue, rValue) => {
          val (lValueType, lValueErrors) = evalTypeOfLValue(lValue)
          val (rValueType, rValueErrors) = evalTypeOfRValue(rValue)
          errors ++= lValueErrors
          errors ++= rValueErrors

          if (!(lValueType equiv rValueType))
            errors += f"Type mismatch: expected $lValueType, got $rValueType"
        }

        case Read(lValue) => {
          val (lValueType, lValueErrors) = evalTypeOfLValue(lValue)
          errors ++= lValueErrors

          lValueType match {
            case IntType() | CharType() => ()
            case _ =>
              errors += f"Type mismatch: expected Int or Char, got $lValueType"
          }
        }

        case Free(expr) => {
          val (exprType, exprTypeErrors) = evalTypeOfExpr(expr)
          errors ++= exprTypeErrors

          exprType match {
            case PairType(_, _) | ArrayType(_) =>
            case _ =>
              errors += f"Type mismatch: expected Array or Pair, got $exprType"
          }
        }

        case Return(expr) => {
          val (exprType, exprTypeErrors) = evalTypeOfExpr(expr)
          errors ++= exprTypeErrors

          (returnType, exprType) match {
            case (Some(returnType), exprType) =>
              if (!(returnType equiv exprType))
                errors += f"Type mismatch: expected $returnType, got $exprType"
            case (None, _) => errors += "Return statement outside function"
          }
        }

        case Exit(expr) => {
          val (exprType, exprTypeErrors) = evalTypeOfExpr(expr)
          errors ++= exprTypeErrors
          exprType match {
            case IntType() =>
            case _ => errors += f"Type mismatch: expected Int, got $exprType"
          }
        }

        case Print(expr) => {
          val (exprType, exprTypeErrors) = evalTypeOfExpr(expr)
          errors ++= exprTypeErrors
        }

        case Println(expr) => {
          val (exprType, exprTypeErrors) = evalTypeOfExpr(expr)
          errors ++= exprTypeErrors
        }

        case If(cond, thenStat, elseStat) => {
          val (condType, condTypeErrors) = evalTypeOfExpr(cond)
          errors ++= condTypeErrors

          condType match {
            case BoolType() => ()
            case _ => errors += f"Type mismatch: expected Bool, got $condType"
          }

          curSymbolTable = symbolTable ++ scopedSymbolTable.toMap

          errors ++= checkStatSemantics(
            curSymbolTable,
            thenStat,
            returnType
          )

          errors ++= checkStatSemantics(
            curSymbolTable,
            elseStat,
            returnType
          )

        }

        case While(cond, doStat) => {
          val (condType, condTypeErrors) = evalTypeOfExpr(cond)
          errors ++= condTypeErrors
          condType match {
            case BoolType() => ()
            case _ => errors += f"Type mismatch: expected Bool, got $condType"
          }

          curSymbolTable = symbolTable ++ scopedSymbolTable.toMap

          errors ++= checkStatSemantics(
            curSymbolTable,
            doStat,
            returnType
          )
        }

        case Scope(scopeStats) => {
          curSymbolTable = symbolTable ++ scopedSymbolTable.toMap

          errors ++= checkStatSemantics(
            curSymbolTable,
            scopeStats,
            returnType
          )
        }

        case _ => errors += "TODO: Implement checkStatSemantics"
      }
    })

    errors.toList
  }

  def evalTypeOfLValue(lValue: LValue)(implicit
      funcTable: Map[Ident, (Type, List[Type])],
      symbolTable: Map[Ident, Type]
  ): (Type, List[String]) = {
    val errors: mutable.ListBuffer[String] = mutable.ListBuffer.empty

    lValue match {
      case ident: Ident => {
        (symbolTable get ident) match {
          case Some(ty) => (ty, errors.toList)
          case None =>
            (
              ErrorType()(NULLPOS),
              (errors += f"Variable $ident not defined").toList
            )
        }
      }
      case ArrayElem(ident, xs) => {
        (symbolTable get ident) match {
          case Some(ArrayType(ty)) => (ty, errors.toList)
          case Some(ty) =>
            (
              ErrorType()(NULLPOS),
              (errors += f"Variable $ident is not an array").toList
            )
          case None =>
            (
              ErrorType()(NULLPOS),
              (errors += f"Variable $ident not defined").toList
            )
        }
      }
      case Fst(l) => {
        evalTypeOfLValue(l) match {
          case (PairType(fstTy, _), lValueErrors) => (fstTy.asType, lValueErrors)
          case (ty, lValueErrors) =>
            (
              ErrorType()(NULLPOS),
              (errors ++= lValueErrors += f"Variable $l is not a pair").toList
            )
        }
      }
      case Snd(l) => {
        evalTypeOfLValue(l) match {
          case (PairType(_, sndTy), lValueErrors) => (sndTy.asType, lValueErrors)
          case (ty, lValueErrors) =>
            (
              ErrorType()(NULLPOS),
              (errors ++= lValueErrors += f"Variable $l is not a pair").toList
            )
        }
      }
    }
  }

  def evalTypeOfRValue(rValue: RValue)(implicit
      funcTable: Map[Ident, (Type, List[Type])],
      symbolTable: Map[Ident, Type]
  ): (Type, List[String]) = {
    val errors: mutable.ListBuffer[String] = mutable.ListBuffer.empty

    rValue match {
      case expr: Expr => evalTypeOfExpr(expr)
      case arrayLit @ ArrayLit(xs) => {
        xs match {
          case Nil =>
            (ArrayType(AnyType()(NULLPOS))(arrayLit.pos), errors.toList)
          case xs @ (head :: tail) => {
            val (expectedArrElemType, headErrors) =
              evalTypeOfExpr(head)
            errors ++= headErrors

            val (actualArrElemType, tailErrors) =
              checkExprs(tail, expectedArrElemType)
            errors ++= tailErrors
            (ArrayType(actualArrElemType)(arrayLit.pos), errors.toList)
          }
        }
      }
      case NewPair(fst, snd) => {
        val (fstType, fstErrors) = evalTypeOfExpr(fst)
        val (sndType, sndErrors) = evalTypeOfExpr(snd)
        errors ++= fstErrors
        errors ++= sndErrors
        (
          PairType(fstType.eraseInnerTypes, sndType.eraseInnerTypes)(NULLPOS),
          errors.toList
        )
      }
      case Call(f, args) => {
        (funcTable get f) match {
          case Some((returnType, paramTypes)) => {
            val (argTypes, argErrors) =
              args.map(evalTypeOfExpr(_)).unzip
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
          case None =>
            (
              ErrorType()(NULLPOS),
              (errors += f"Function $f not defined").toList
            )
        }

      }
      case Fst(lValue) => {
        val (exprType, error) = evalTypeOfLValue(lValue)
        errors ++= error
        exprType match {
          case PairType(fstType, _) => (fstType.asType, errors.toList)
          case _                    => (ErrorType()(NULLPOS), errors.toList)
        }
      }
      case Snd(lValue) => {
        val (exprType, error) = evalTypeOfLValue(lValue)
        errors ++= error
        exprType match {
          case PairType(_, sndType) => (sndType.asType, errors.toList)
          case _                    => (ErrorType()(NULLPOS), errors.toList)
        }
      }
    }
  }

  def evalTypeOfExpr(
      expr: Expr
  )(implicit symbolTable: Map[Ident, Type]): (Type, List[String]) = {
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
      case ArrayElem(ident, xs) => {
        def getArrayTypeRank(ty: Type): Int = {
          ty match {
            case ArrayType(innerTy) => 1 + getArrayTypeRank(innerTy)
            case _                  => 0
          }
        }

        checkExprs(xs, IntType()(NULLPOS)) match {
          case (IntType(), errors) => {
            symbolTable get ident match {
              case Some(t @ ArrayType(_)) => {
                if (xs.length <= getArrayTypeRank(t))
                  (t, errors)
                else
                  (ErrorType()(NULLPOS), List("Array dimension mismatch"))
              }
              case Some(_) =>
                (ErrorType()(NULLPOS), errors :+ "Variable is not an array")
              case None =>
                (ErrorType()(NULLPOS), errors :+ "Variable not defined")
            }
          }
          case (t, errors) =>
            (ErrorType()(NULLPOS), errors :+ "Array index is not an integer")
        }
      }
      case not @ Not(x)    => checkExprType(x, BoolType()(not.pos))
      case neg @ Negate(x) => checkExprType(x, IntType()(neg.pos))
      case len @ Len(x) =>
        checkExprType(x, ArrayType(AnyType()(len.pos))(len.pos))
      case ord @ Ord(x) => checkExprType(x, CharType()(ord.pos))
      case chr @ Chr(x) => checkExprType(x, IntType()(chr.pos))
      case mul @ Mult(x, y) =>
        check2ExprType(Set(IntType()(NULLPOS)), x, y, IntType()(mul.pos))
      case div @ Div(x, y) =>
        check2ExprType(Set(IntType()(NULLPOS)), x, y, IntType()(div.pos))
      case mod @ Mod(x, y) =>
        check2ExprType(Set(IntType()(NULLPOS)), x, y, IntType()(mod.pos))
      case add @ Add(x, y) =>
        check2ExprType(Set(IntType()(NULLPOS)), x, y, IntType()(add.pos))
      case sub @ Sub(x, y) =>
        check2ExprType(Set(IntType()(NULLPOS)), x, y, IntType()(sub.pos))
      case eq @ Equal(x, y) =>
        check2ExprType(Set(AnyType()(NULLPOS)), x, y, BoolType()(eq.pos))
      case neq @ NotEqual(x, y) =>
        check2ExprType(Set(AnyType()(NULLPOS)), x, y, BoolType()(neq.pos))
      case lt @ LT(x, y) =>
        check2ExprType(
          Set(IntType()(NULLPOS), CharType()(NULLPOS)),
          x,
          y,
          BoolType()(lt.pos)
        )
      case lte @ LTE(x, y) =>
        check2ExprType(
          Set(IntType()(NULLPOS), CharType()(NULLPOS)),
          x,
          y,
          BoolType()(lte.pos)
        )
      case and @ And(x, y) =>
        check2ExprType(Set(BoolType()(NULLPOS)), x, y, BoolType()(and.pos))
      case or @ Or(x, y) =>
        check2ExprType(Set(BoolType()(NULLPOS)), x, y, BoolType()(or.pos))
      case gt @ GT(x, y) =>
        check2ExprType(
          Set(IntType()(NULLPOS), CharType()(NULLPOS)),
          x,
          y,
          BoolType()(gt.pos)
        )
      case gte @ GTE(x, y) =>
        check2ExprType(
          Set(IntType()(NULLPOS), CharType()(NULLPOS)),
          x,
          y,
          BoolType()(gte.pos)
        )
      case Bracket(x) => evalTypeOfExpr(x)
    }
  }

  // Check that an expression is of a certain type
  private def checkExprType(
      expr: Expr,
      expectedType: Type
  )(implicit symbolTable: Map[Ident, Type]): (Type, List[String]) = {
    val (exprType, error) = evalTypeOfExpr(expr)

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
      argTypes: Set[Type],
      expr1: Expr,
      expr2: Expr,
      retType: Type
  )(implicit
      symbolTable: Map[Ident, Type]
  ): (Type, List[String]) = {
    val (expr1Type, error1) = evalTypeOfExpr(expr1)
    val (expr2Type, error2) = evalTypeOfExpr(expr2)
    val errors = error1 ++ error2

    if (!argTypes.exists(expr1Type equiv _)) {
      (
        ErrorType()(NULLPOS),
        errors :+ f"Expected type of arg1 to be one of $argTypes but got $expr1Type instead"
      )
    } else if (!argTypes.exists(expr2Type equiv _)) {
      (
        ErrorType()(NULLPOS),
        errors :+ f"Expected type of arg2 to be one of $argTypes but got $expr2Type instead"
      )
    } else if (!((expr1Type equiv expr2Type) || (expr2Type equiv expr1Type))) {
      (
        ErrorType()(NULLPOS),
        errors :+ f"Expected arg1 and arg2 to have the same type but got $expr1Type and $expr2Type instead"
      )
    } else {
      (retType, errors)
    }
  }

  // Check that a list of expressions are of a certain type
  private def checkExprs(
      exprs: List[Expr],
      expectedType: Type
  )(implicit symbolTable: Map[Ident, Type]): (Type, List[String]) = {
    exprs match {
      case Nil => (expectedType, Nil)
      case _ => {
        val evals = exprs.map(checkExprType(_, expectedType))
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
  }

}
