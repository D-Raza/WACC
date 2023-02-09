package wacc.frontend

import wacc.AST._
import wacc.frontend.errors._
import java.io.File
import scala.collection.mutable

object SemanticAnalyser {

  def checkProgramSemantics(
      program: Program
  )(implicit source: File): List[WACCError] = {
    val errors: mutable.ListBuffer[WACCError] = mutable.ListBuffer.empty
    val functionDefs: mutable.Map[Ident, (Type, List[Type])] = mutable.Map.empty

    program match {
      case Program(funcs, stats) => {
        funcs.foreach(func => {
          if (functionDefs contains func.ident)
            errors += RedefinedFunction.genError(func.ident)

          functionDefs += (func.ident -> (func.ty, func.paramList.map(_.ty)))
        })
        implicit val funcTable: Map[Ident, (Type, List[Type])] =
          functionDefs.toMap

        funcs.foreach(func => {
          val paramTable: mutable.Map[Ident, Type] = mutable.Map.empty

          func.paramList.foreach(param => {
            if (paramTable contains param.ident)
              errors += RedefinedVariableError.genError(param.ident)
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
      source: File,
      funcTable: Map[Ident, (Type, List[Type])]
  ): List[WACCError] = {
    val errors: mutable.ListBuffer[WACCError] = mutable.ListBuffer.empty

    val scopedSymbolTable: mutable.Map[Ident, Type] = mutable.Map.empty
    implicit var curSymbolTable: Map[Ident, Type] =
      symbolTable ++ scopedSymbolTable.toMap

    stats.foreach(stat => {
      stat match {
        case Skip() => ()

        case Declare(ty, ident, rvalue) => {
          if (scopedSymbolTable contains ident) {
            errors += RedefinedVariableError.genError(ident)
          } else {
            scopedSymbolTable += (ident -> ty)
            curSymbolTable = symbolTable ++ scopedSymbolTable.toMap
          }

          val (rValueType, rValueErrors) = evalTypeOfRValue(rvalue, stat)
          errors ++= rValueErrors
          if (!(rValueType equiv ty))
            errors += TypeError.genError(
              rValueType,
              Set(ty),
              rvalue.pos,
              s"declaration of variable ${ident.name}"
            )
        }

        case Assign(lValue, rValue) => {
          val (lValueType, lValueErrors) = evalTypeOfLValue(lValue)
          val (rValueType, rValueErrors) = evalTypeOfRValue(rValue, stat)
          errors ++= lValueErrors
          errors ++= rValueErrors

          (lValueType, rValueType) match {
            case (UnknownType(), UnknownType()) =>
              errors += TypeError.genError(
                rValueType,
                Set(lValueType),
                rValue.pos,
                "pair element assignment"
              )
            case _ => {
              if (!(lValueType equiv rValueType))
                errors += TypeError.genError(
                  rValueType,
                  Set(lValueType),
                  rValue.pos,
                  "assignment"
                )
            }
          }
        }

        case Read(lValue) => {
          val (lValueType, lValueErrors) = evalTypeOfLValue(lValue)
          errors ++= lValueErrors

          lValueType match {
            case IntType() | CharType() => ()
            case _ =>
              errors += TypeError.genError(
                lValueType,
                Set(IntType()(NULLPOS), CharType()(NULLPOS)),
                lValue.pos,
                "read"
              )
          }
        }

        case Free(expr) => {
          val (exprType, exprTypeErrors) = evalTypeOfExpr(expr)
          errors ++= exprTypeErrors

          exprType match {
            case PairType(_, _) | ArrayType(_) =>
            case _ =>
              errors += TypeError.genError(
                exprType,
                Set(
                  ArrayType(AnyType()(NULLPOS))(NULLPOS),
                  PairType(AnyType()(NULLPOS), AnyType()(NULLPOS))(NULLPOS)
                ),
                expr.pos,
                "free"
              )
          }
        }

        case Return(expr) => {
          val (exprType, exprTypeErrors) = evalTypeOfExpr(expr)
          errors ++= exprTypeErrors

          (returnType, exprType) match {
            case (Some(returnType), exprType) =>
              if (!(returnType equiv exprType))
                errors += TypeError.genError(
                  exprType,
                  Set(returnType),
                  expr.pos,
                  "return"
                )
            case (None, _) => errors += UnexpectedReturnError.genError(stat)
          }
        }

        case Exit(expr) => {
          val (exprType, exprTypeErrors) = evalTypeOfExpr(expr)
          errors ++= exprTypeErrors
          exprType match {
            case IntType() =>
            case _ =>
              errors += TypeError.genError(
                exprType,
                Set(IntType()(NULLPOS)),
                expr.pos,
                "exit"
              )
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
            case _ =>
              errors += TypeError.genError(
                condType,
                Set(BoolType()(NULLPOS)),
                cond.pos,
                "if block"
              )
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
            case _ =>
              errors += TypeError.genError(
                condType,
                Set(BoolType()(NULLPOS)),
                cond.pos,
                "while block"
              )
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
      }
    })

    errors.toList
  }

  def evalTypeOfLValue(lValue: LValue)(implicit
      source: File,
      funcTable: Map[Ident, (Type, List[Type])],
      symbolTable: Map[Ident, Type]
  ): (Type, List[WACCError]) = {
    val errors: mutable.ListBuffer[WACCError] = mutable.ListBuffer.empty

    lValue match {
      case ident: Ident => {
        (symbolTable get ident) match {
          case Some(ty) => (ty, errors.toList)
          case None =>
            (
              ErrorType()(ident.pos),
              (errors += UndefinedVariableError.genError(ident)).toList
            )
        }
      }
      case ArrayElem(ident, xs) => {
        (symbolTable get ident) match {
          case Some(ArrayType(ty)) => (ty, errors.toList)
          case Some(ty) =>
            (
              ErrorType()(ident.pos),
              (errors += TypeError.genError(
                ty,
                Set(ArrayType(AnyType()(NULLPOS))(NULLPOS)),
                ident.pos,
                "array index"
              )).toList
            )
          case None =>
            (
              ErrorType()(ident.pos),
              (errors += UndefinedVariableError.genError(ident)).toList
            )
        }
      }
      case Fst(l) => {
        evalTypeOfLValue(l) match {
          case (PairType(fstTy, _), lValueErrors) =>
            (fstTy.asType, lValueErrors)
          case (ty, lValueErrors) =>
            (
              ErrorType()(l.pos),
              (errors ++= lValueErrors += TypeError.genError(
                ty,
                Set(PairType(AnyType()(NULLPOS), AnyType()(NULLPOS))(NULLPOS)),
                l.pos,
                "fst"
              )).toList
            )
        }
      }
      case Snd(l) => {
        evalTypeOfLValue(l) match {
          case (PairType(_, sndTy), lValueErrors) =>
            (sndTy.asType, lValueErrors)
          case (ty, lValueErrors) =>
            (
              ErrorType()(l.pos),
              (errors ++= lValueErrors += TypeError.genError(
                ty,
                Set(PairType(AnyType()(NULLPOS), AnyType()(NULLPOS))(NULLPOS)),
                l.pos,
                "snd"
              )).toList
            )
        }
      }
    }
  }

  def evalTypeOfRValue(rValue: RValue, stat: Stat)(implicit
      source: File,
      funcTable: Map[Ident, (Type, List[Type])],
      symbolTable: Map[Ident, Type]
  ): (Type, List[WACCError]) = {
    val errors: mutable.ListBuffer[WACCError] = mutable.ListBuffer.empty

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
      case c @ Call(f, args) => {
        (funcTable get f) match {
          case Some((returnType, paramTypes)) => {
            val (argTypes, argErrors) =
              args.map(evalTypeOfExpr(_)).unzip
            errors ++= argErrors.flatten

            val (returnType, paramTypes) = funcTable(f)
            if (argTypes.length != paramTypes.length)
              errors += NoArgs.genError(f, argTypes.length, paramTypes.length)

            argTypes.zip(paramTypes).zipWithIndex.foreach {
              case ((argType, paramType), i) => {
                if (!(argType equiv paramType))
                  errors += TypeError.genError(
                    argType,
                    Set(paramType),
                    args.toIndexedSeq(i).pos,
                    s"function call to ${f}"
                  ) /* NEED TO INFORM THE USER WHICH ARGUMENT'S TYPE MISMATCHES */
              }
            }

            (returnType, errors.toList)
          }
          case None =>
            (
              ErrorType()(c.pos),
              (errors += UndefinedFunction.genError(f)).toList
            )
        }

      }
      case Fst(lValue) => {
        val (exprType, error) = evalTypeOfLValue(lValue)
        errors ++= error
        exprType match {
          case PairType(fstType, _) => (fstType.asType, errors.toList)
          case _                    => (ErrorType()(lValue.pos), errors.toList)
        }
      }
      case Snd(lValue) => {
        val (exprType, error) = evalTypeOfLValue(lValue)
        errors ++= error
        exprType match {
          case PairType(_, sndType) => (sndType.asType, errors.toList)
          case _                    => (ErrorType()(lValue.pos), errors.toList)
        }
      }
    }
  }

  def evalTypeOfExpr(
      expr: Expr
  )(implicit
      source: File,
      symbolTable: Map[Ident, Type]
  ): (Type, List[WACCError]) = {
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
          (symbolTable(ident).positioned(ident.pos), Nil)
        } else {
          (ErrorType()(ident.pos), List(UndefinedVariableError.genError(ident)))
        }
      }
      case ArrayElem(ident, xs) => {
        def getArrayTypeRank(ty: Type): Int = {
          ty match {
            case ArrayType(innerTy) => 1 + getArrayTypeRank(innerTy)
            case _                  => 0
          }
        }

        val errors: mutable.ListBuffer[WACCError] = mutable.ListBuffer.empty

        (symbolTable get ident) match {
          case Some(t @ ArrayType(innerType)) => {
            val (argTypes, argErrors) =
              xs.map(evalTypeOfExpr(_)).unzip
            errors ++= argErrors.flatten

            if (xs.length > getArrayTypeRank(t))
              (
                ErrorType()(ident.pos),
                (errors += ArrayDimensionMismatch.genError(
                  xs.length,
                  getArrayTypeRank(t),
                  ident.pos
                )).toList
              )

            argTypes.zipWithIndex.foreach {
              case (argType, i) => {
                if (!(argType equiv IntType()(NULLPOS)))
                  (
                    ErrorType()(ident.pos),
                    errors += TypeError.genError(
                      argType,
                      Set(IntType()(NULLPOS)),
                      xs.toIndexedSeq(i).pos,
                      s"array access for ${ident}"
                    )
                  )
              }
            }

            (innerType.positioned(ident.pos), errors.toList)
          }
          case Some(ot) =>
            (
              ErrorType()(ident.pos),
              (errors += TypeError.genError(
                ot,
                Set(ArrayType(AnyType()(NULLPOS))(NULLPOS)),
                ident.pos,
                s"array access for ${ident}"
              )).toList
            )
          case None =>
            (
              ErrorType()(ident.pos),
              (errors += UndefinedVariableError.genError(ident)).toList
            )
        }

      }
      case not @ Not(x) =>
        checkExprType(x, BoolType()(not.pos), BoolType()(not.pos))
      case neg @ Neg(x) =>
        checkExprType(x, IntType()(neg.pos), IntType()(neg.pos))
      case len @ Len(x) =>
        checkExprType(
          x,
          ArrayType(AnyType()(len.pos))(len.pos),
          IntType()(len.pos)
        )
      case ord @ Ord(x) =>
        checkExprType(x, CharType()(ord.pos), IntType()(ord.pos))
      case chr @ Chr(x) =>
        checkExprType(x, IntType()(chr.pos), CharType()(chr.pos))
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
      expectedType: Type,
      retType: Type // The type of the return value
  )(implicit
      source: File,
      symbolTable: Map[Ident, Type]
  ): (Type, List[WACCError]) = {
    val (exprType, error) = evalTypeOfExpr(expr)

    if (exprType equiv expectedType) {
      (retType, error)
    } else {
      (
        ErrorType()(expr.pos),
        error :+ TypeError.genError(exprType, Set(expectedType), expr.pos, "")
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
      source: File,
      symbolTable: Map[Ident, Type]
  ): (Type, List[WACCError]) = {
    val (expr1Type, error1) = evalTypeOfExpr(expr1)
    val (expr2Type, error2) = evalTypeOfExpr(expr2)
    val errors = error1 ++ error2

    if (!argTypes.exists(expr1Type equiv _)) {
      (
        ErrorType()(expr1.pos),
        errors :+ TypeError.genError(expr1Type, Set(expr2Type), expr1.pos, "")
      )
    } else if (!argTypes.exists(expr2Type equiv _)) {
      (
        ErrorType()(expr2.pos),
        errors :+ TypeError.genError(expr2Type, Set(expr1Type), expr2.pos, "")
      )
    } else if (!((expr1Type equiv expr2Type) || (expr2Type equiv expr1Type))) {
      (
        ErrorType()(expr1.pos),
        errors :+ TypeError.genError(expr1Type, Set(expr2Type), expr2.pos, "")
      )
    } else {
      (retType, errors)
    }
  }

  // Check that a list of expressions are of a certain type
  private def checkExprs(
      exprs: List[Expr],
      expectedType: Type
  )(implicit
      source: File,
      symbolTable: Map[Ident, Type]
  ): (Type, List[WACCError]) = {
    exprs match {
      case Nil => (expectedType, Nil)
      case _ => {
        val evals = exprs.map(checkExprType(_, expectedType, expectedType))
        val types = evals.map(_._1)
        val errors = evals.flatMap(_._2)
        if (types.distinct.length == 1 && (types.head equiv expectedType)) {
          (types.head, errors)
        } else {
          (
            (
              ErrorType()(exprs.head.pos),
              errors
            )
          )
        }
      }
    }
  }

}
