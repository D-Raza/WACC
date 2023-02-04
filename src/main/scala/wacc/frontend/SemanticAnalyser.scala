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

  // def evalTypeOfExpr(expr: Expr, symbolTable: Map[Ident, Type]): (Type, List[String]) = {
  //     expr match {
  //       case Ident(name) =>
  //       case ArrayElem(ident, xs) =>
  //       case IntegerLiter(x) =>
  //       case BoolLiter(x) =>
  //       case CharLiter(x) =>
  //       case StrLiter(x) =>
  //       case PairLiter() =>
  //       case Mult(x, y) =>
  //       case Div(x, y) =>
  //       case Mod(x, y) =>
  //       case Add(x, y) =>
  //       case Sub(x, y) =>
  //       case Equal(x, y) =>
  //       case NotEqual(x, y) =>
  //       case LT(x, y) =>
  //       case LTE(x, y) =>
  //       case And(x, y) =>
  //       case Or(x, y) =>
  //       case GT(x, y) =>
  //       case GTE(x, y) =>
  //       case Not(x) =>
  //       case Negate(x) =>
  //       case Len(x) =>
  //       case Ord(x) =>
  //       case Chr(x) =>
  //       case Bracket(x) =>
  //     }
  //   }
}
