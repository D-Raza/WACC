package wacc.backend

import wacc.AST._
import wacc.backend.Globals.{PAIR_SIZE, WORD_SIZE}

import scala.collection.mutable

object CodeGenerator {
  // Compiles the program
  def compileProgram(programNode: Program, codeGenState: CodeGeneratorState)(
      implicit instructions: mutable.ListBuffer[Instruction]
  ): CodeGeneratorState = {
    var newCodeGenState = codeGenState

    // Compiles each function declaration in the program
    programNode.funcs.foreach(func =>
      newCodeGenState = newCodeGenState.addFunctionName(func.ident.name)
    )
    programNode.funcs.foreach(func => compileFunc(func, newCodeGenState))

    instructions.addAll(
      List(
        Directive("global main"),
        Label("main"),
        Push(List(FP, LR)),
        Move(FP, SP)
      )
    )

    newCodeGenState = newCodeGenState.copy(stackPointerOffset =
      newCodeGenState.stackPointerOffset + 4
    )

    // Compiles each statement in the program
    programNode.stat.foreach(stat =>
      newCodeGenState = compileStat(stat, newCodeGenState)
    )

    // Set exit code as 0
    instructions.addOne(Move(R0, ImmVal(0)))

    instructions.addAll(
      List(
        Pop(List(FP, PC))
      )
    )

    newCodeGenState.copy(stackPointerOffset =
      newCodeGenState.stackPointerOffset - 4
    )
  }

  // Compiles function declaration
  def compileFunc(funcNode: Func, codeGenState: CodeGeneratorState)(implicit
      instructions: mutable.ListBuffer[Instruction]
  ): CodeGeneratorState = {
    var newCodeGenState = codeGenState

    instructions.addAll(
      List(
        Push(List(FP, LR)),
        Move(FP, SP)
      )
    )

    // Compile each of the parameters in the function declaration
    funcNode.paramList.foreach(param =>
      newCodeGenState = compileParam(param, newCodeGenState)
    )

    instructions.addAll(
      List(
        Label("wacc_" + funcNode.ident.name),
        Push(List(LR))
      )
    )

    newCodeGenState = newCodeGenState.copy(stackPointerOffset =
      newCodeGenState.stackPointerOffset + 4
    )

    // Needed for restoring stack pointer after compiling the body of the function declaration
    val newIdentToOffset =
      newCodeGenState.identToOffset + ("originalSP" -> newCodeGenState.stackPointerOffset)
    newCodeGenState = newCodeGenState.copy(identToOffset = newIdentToOffset)

    // Compile each of the statements in the function declaration's body
    funcNode.stats.foreach(stat =>
      newCodeGenState = compileStat(stat, newCodeGenState)
    )

    newCodeGenState = newCodeGenState.copy(identToOffset =
      newCodeGenState.identToOffset - "originalSP"
    )

    instructions.addOne(Pop(List(FP)))

    instructions.addAll(
      List(
        Pop(List(FP, PC)),
        Directive(".ltorg")
      )
    )

    val newStackPointerOffset =
      newCodeGenState.stackPointerOffset - newCodeGenState.usedStackSize - 4
    codeGenState.copy(
      usedStackSize = 0,
      stackPointerOffset = newStackPointerOffset
    )
  }

  // Compiles function parameter
  def compileParam(
      paramNode: Param,
      codeGenState: CodeGeneratorState
  ): CodeGeneratorState = {
    val newIdentToOffset =
      codeGenState.identToOffset + (paramNode.ident.name -> (paramNode.ty.size + codeGenState.stackPointerOffset))
    val newUsedStackSize = codeGenState.usedStackSize + paramNode.ty.size
    val newStackPointerOffset =
      codeGenState.stackPointerOffset + paramNode.ty.size

    codeGenState.copy(
      identToOffset = newIdentToOffset,
      usedStackSize = newUsedStackSize,
      stackPointerOffset = newStackPointerOffset
    )
  }

  // Compiles function call
  def compileFunctionCall(funcCallNode: Call, codeGenState: CodeGeneratorState)(
      implicit instructions: mutable.ListBuffer[Instruction]
  ): CodeGeneratorState = {
    var newCodeGenState = codeGenState
    val argsSize =
      funcCallNode.args.foldLeft(0)((sum: Int, expr: Expr) => sum + expr.size)
    val resReg = newCodeGenState.getResReg

    // Compile each of the arguments
    funcCallNode.args.foreach(arg =>
      newCodeGenState = compileExpression(arg, newCodeGenState)
    )

    instructions += BranchAndLink("wacc_" + funcCallNode.x.name)

    if (argsSize > 0) {
      print("argsSize: " + argsSize)
      instructions.addAll(
        List(
          // Set the stack pointer back to its original value
          AddInstr(SP, SP, ImmVal(argsSize))
        )
      )
    }

    instructions.addAll(
      List(
        // Move result to result register
        Move(resReg, R0)
      )
    )

    val newStackPointerOffset = newCodeGenState.stackPointerOffset - argsSize
    val newAvailableRegs = newCodeGenState.availableRegs.tail
    newCodeGenState.copy(
      availableRegs = newAvailableRegs,
      stackPointerOffset = newStackPointerOffset
    )
  }

  // Compiles expression
  def compileExpression(exprNode: Expr, codeGenState: CodeGeneratorState)(
      implicit instructions: mutable.ListBuffer[Instruction]
  ): CodeGeneratorState = {
    var newCodeGenState = codeGenState
    val resReg = newCodeGenState.getResReg
    val operand1Reg = newCodeGenState.getResReg

    exprNode match {
      // Case for binary expressions
      case Add(_, _) | Sub(_, _) | Mult(_, _) | Div(_, _) | Mod(_, _) |
          And(_, _) | Or(_, _) | LT(_, _) | LTE(_, _) | GT(_, _) | GTE(_, _) |
          Equal(_, _) | NotEqual(_, _) => {
        val operand2Reg = newCodeGenState.getNonResReg

        /* Compile the first and second expression in the binary expression,
           and add the corresponding instructions needed to instructions list */
        exprNode match {
          // TODO: Overflow
          case Mult(x, y) =>
            newCodeGenState = compileExpression(x, newCodeGenState)
            newCodeGenState = compileExpression(y, newCodeGenState)
            instructions += SMull(
              operand1Reg,
              operand2Reg,
              operand1Reg,
              operand2Reg
            )

          // TODO
          case Div(x, y) =>
            newCodeGenState = compileExpression(x, newCodeGenState)
            newCodeGenState = compileExpression(y, newCodeGenState)
            instructions.addAll(
              List(
                BranchAndLink(
                  "__aeabi_idiv"
                ), // signed __aeabi_idiv(signed numerator, signed denominator)
                Move(resReg, R0)
              )
            )

          // TODO
          case Mod(x, y) =>
            newCodeGenState = compileExpression(x, newCodeGenState)
            newCodeGenState = compileExpression(y, newCodeGenState)
            instructions.addAll(
              List(
                BranchAndLink(
                  "__aeabi_idivmod"
                ), // signed __aeabi_idivmod(signed numerator, signed denominator)
                Move(resReg, R1)
              )
            )

          case Add(x, y) =>
            newCodeGenState = compileExpression(x, newCodeGenState)
            newCodeGenState = compileExpression(y, newCodeGenState)
            instructions += AddInstr(resReg, operand1Reg, operand2Reg)

          case And(x, y) =>
            newCodeGenState = compileExpression(x, newCodeGenState)
            newCodeGenState = compileExpression(y, newCodeGenState)
            instructions += AndInstr(resReg, operand1Reg, operand2Reg)

          case Or(x, y) =>
            newCodeGenState = compileExpression(x, newCodeGenState)
            newCodeGenState = compileExpression(y, newCodeGenState)
            instructions += OrrInstr(resReg, operand1Reg, operand2Reg)

          case Sub(x, y) =>
            newCodeGenState = compileExpression(x, newCodeGenState)
            newCodeGenState = compileExpression(y, newCodeGenState)
            instructions += SubInstr(resReg, operand1Reg, operand2Reg)

          case LT(x, y) =>
            newCodeGenState = compileExpression(x, newCodeGenState)
            newCodeGenState = compileExpression(y, newCodeGenState)
            instructions.addAll(
              List(
                Cmp(operand1Reg, operand2Reg),
                Move(resReg, ImmVal(1), Condition.LT),
                Move(resReg, ImmVal(0), Condition.GE)
              )
            )

          case LTE(x, y) =>
            newCodeGenState = compileExpression(x, newCodeGenState)
            newCodeGenState = compileExpression(y, newCodeGenState)
            instructions.addAll(
              List(
                Cmp(operand1Reg, operand2Reg),
                Move(resReg, ImmVal(1), Condition.LE),
                Move(resReg, ImmVal(0), Condition.GT)
              )
            )

          case GT(x, y) =>
            newCodeGenState = compileExpression(x, newCodeGenState)
            newCodeGenState = compileExpression(y, newCodeGenState)
            instructions.addAll(
              List(
                Cmp(operand1Reg, operand2Reg),
                Move(resReg, ImmVal(1), Condition.GT),
                Move(resReg, ImmVal(0), Condition.LE)
              )
            )

          case GTE(x, y) =>
            newCodeGenState = compileExpression(x, newCodeGenState)
            newCodeGenState = compileExpression(y, newCodeGenState)
            instructions.addAll(
              List(
                Cmp(operand1Reg, operand2Reg),
                Move(resReg, ImmVal(1), Condition.GE),
                Move(resReg, ImmVal(0), Condition.LT)
              )
            )

          case NotEqual(x, y) =>
            newCodeGenState = compileExpression(x, newCodeGenState)
            newCodeGenState = compileExpression(y, newCodeGenState)
            instructions.addAll(
              List(
                Cmp(operand1Reg, operand2Reg),
                Move(resReg, ImmVal(1), Condition.NE),
                Move(resReg, ImmVal(0), Condition.EQ)
              )
            )

          case Equal(x, y) =>
            newCodeGenState = compileExpression(x, newCodeGenState)
            newCodeGenState = compileExpression(y, newCodeGenState)
            instructions.addAll(
              List(
                Cmp(operand1Reg, operand2Reg),
                Move(resReg, ImmVal(1), Condition.EQ),
                Move(resReg, ImmVal(0), Condition.NE)
              )
            )

          case _ => ()
        }

        // Register for operand2 is now available for use
        val newAvailableRegs = operand2Reg +: newCodeGenState.availableRegs
        newCodeGenState = newCodeGenState.copy(availableRegs = newAvailableRegs)
      }

      // Case for unary expressions
      case Not(_) | Neg(_) | Len(_) | Ord(_) | Chr(_) => {
        /* Compile the first expression in the unary expression,
         and add add the corresponding instructions needed to instructions list */
        exprNode match {
          case Not(x) =>
            newCodeGenState = compileExpression(x, newCodeGenState)
            instructions += XorInstr(resReg, resReg, ImmVal(1))

          case Neg(x) =>
            newCodeGenState = compileExpression(x, newCodeGenState)
            instructions += Rsb(resReg, resReg, ImmVal(0))

          case Len(x) =>
            newCodeGenState = compileExpression(x, newCodeGenState)
            instructions += Load(resReg, OffsetMode(resReg))

          case Ord(x) =>
            newCodeGenState = compileExpression(x, newCodeGenState)

          case Chr(x) =>
            newCodeGenState = compileExpression(x, newCodeGenState)

          case _ => ()
        }
      }

      // Case for expression literals
      case IntegerLiter(_) | BoolLiter(_) | CharLiter(_) | StrLiter(_) |
          Null() => {
        exprNode match {
          case IntegerLiter(x) =>
            instructions += Load(resReg, LoadImmVal(x))

          case BoolLiter(x) =>
            instructions += Load(resReg, LoadImmVal(if (x) 1 else 0))

          case CharLiter(x) =>
            instructions += Move(resReg, ImmChar(x))

          case StrLiter(x) =>
          // TODO

          case Null() =>
            instructions += Load(resReg, LoadImmVal(0))

          case _ => ()
        }

        newCodeGenState = newCodeGenState.copy(availableRegs =
          newCodeGenState.availableRegs.tail
        )
      }

      case _ => ()
    }

    newCodeGenState
  }

  def compileStat(statNode: Stat, codeGenState: CodeGeneratorState)(implicit
      instructions: mutable.ListBuffer[Instruction]
  ): CodeGeneratorState = {
    var newCodeGenState = codeGenState

    statNode match {
      case Skip() =>
        newCodeGenState

      case Assign(lValue, rValue) => {
        newCodeGenState = compileRValue(rValue, newCodeGenState)
        newCodeGenState = compileLValue(lValue, newCodeGenState)
        instructions += Store(
          newCodeGenState.getResReg,
          OffsetMode(newCodeGenState.getResReg)
        )
      }

      case Declare(ty, x, y) =>
        val resReg = newCodeGenState.getResReg

        newCodeGenState = compileRValue(y, newCodeGenState)

        instructions.addAll(
          List(
            SubInstr(FP, FP, ImmVal(ty.size)), // SP, SP, -ty.size
            Store(resReg, OffsetMode(FP)) // OffsetMode(SP)
          )
        )

        val newStackPointerOffset = newCodeGenState.stackPointerOffset + ty.size
        newCodeGenState = newCodeGenState.copy(
          usedStackSize = newCodeGenState.usedStackSize + ty.size,
          identToOffset =
            newCodeGenState.identToOffset + (x.name -> newStackPointerOffset),
          stackPointerOffset = newStackPointerOffset
        )

        val newAvailableRegs = resReg +: newCodeGenState.availableRegs
        newCodeGenState = newCodeGenState.copy(availableRegs = newAvailableRegs)

      case Read(_) =>
      // TODO

      case Free(_) =>
      // TODO

      case Return(expr) =>
        val resReg = newCodeGenState.getResReg
        newCodeGenState = compileExpression(expr, newCodeGenState)

        if (resReg != R0)
          instructions += Move(R0, resReg)

        val stackPointerOffsetDiff =
          newCodeGenState.stackPointerOffset - newCodeGenState.getIdentOffset(
            "originalSP"
          )
        if (stackPointerOffsetDiff > 0) {
          instructions += AddInstr(
            SP,
            SP,
            ImmVal(stackPointerOffsetDiff)
          )
        }

        val newAvailableRegs = resReg +: newCodeGenState.availableRegs
        val newStackPointerOffset = newCodeGenState.getIdentOffset("originalSP")
        newCodeGenState = newCodeGenState.copy(
          availableRegs = newAvailableRegs,
          stackPointerOffset = newStackPointerOffset
        )

      case Exit(expr) =>
        val resReg = newCodeGenState.getResReg

        newCodeGenState = compileExpression(expr, newCodeGenState)

        if (resReg != R0)
          instructions += Move(R0, resReg)

        instructions += BranchAndLink("exit")

        val newAvailableRegs = resReg +: newCodeGenState.availableRegs
        newCodeGenState = newCodeGenState.copy(availableRegs = newAvailableRegs)

      case Print(_) =>
        // TODO
        instructions.addAll(
          List(
            BranchAndLink("TODO_print")
          )
        )

      case Println(_) =>
        // TODO
        instructions.addAll(
          List(
            BranchAndLink("TODO_print")
          )
        )

      case ifStatNode @ If(_, _, _) =>
        newCodeGenState = compileIfStat(ifStatNode, newCodeGenState)

      case While(cond, bodyStat) =>
        val uniqueWhileName = "while_" + codeGenState.getNewLabelId;
        val condLabel = uniqueWhileName + "_cond"
        val bodyLabel = uniqueWhileName + "_body"
        instructions += Branch(condLabel)

        instructions += Label(bodyLabel)
        bodyStat.foreach(stat =>
          newCodeGenState = compileStatWithNewScope(stat, newCodeGenState)
        )

        val condReg = newCodeGenState.getResReg

        instructions += Label(condLabel)
        newCodeGenState = compileExpression(cond, newCodeGenState)

        instructions.addAll(
          List(
            Cmp(condReg, ImmVal(1)),
            Branch(bodyLabel, Condition.EQ)
          )
        )

        val newAvailableRegs = condReg +: newCodeGenState.availableRegs
        newCodeGenState = newCodeGenState.copy(availableRegs = newAvailableRegs)

      case Scope(stats) =>
        stats.foreach(stat =>
          newCodeGenState = compileStatWithNewScope(stat, newCodeGenState)
        ) // TODO - implement compileBlock or similar
    }

    newCodeGenState
  }

  // Compiles 'if-then-else' statements
  def compileIfStat(ifNode: If, codeGenState: CodeGeneratorState)(implicit
      instructions: mutable.ListBuffer[Instruction]
  ): CodeGeneratorState = {
    val condReg = codeGenState.getResReg

    // Compile condition
    var newCodeGenState = compileExpression(ifNode.cond, codeGenState)

    val newAvailableRegs = condReg +: newCodeGenState.availableRegs
    newCodeGenState = newCodeGenState.copy(availableRegs = newAvailableRegs)

    val thenLabel = "l_" + newCodeGenState.getNewLabelId
    val endLabel = "l_" + newCodeGenState.getNewLabelId

    instructions.addAll(
      List(
        Cmp(condReg, ImmVal(1)),
        Branch(thenLabel, Condition.EQ)
      )
    )

    // Compile else statement
    ifNode.elseStat.foreach(stat =>
      newCodeGenState = compileStatWithNewScope(stat, newCodeGenState)
    )

    instructions.addAll(
      List(
        Branch(endLabel),
        Label(thenLabel)
      )
    )

    // Compile then statement
    ifNode.thenStat.foreach(stat =>
      newCodeGenState = compileStatWithNewScope(stat, newCodeGenState)
    )

    instructions += Label(endLabel)

    newCodeGenState
  }

  def compileRValue(rValueNode: RValue, codeGenState: CodeGeneratorState)(
      implicit instructions: mutable.ListBuffer[Instruction]
  ): CodeGeneratorState = {
    var newCodeGenState = codeGenState

    rValueNode match {
      case ArrayLit(xs) =>
        newCodeGenState =
          compileExpression(rValueNode.asInstanceOf[Expr], newCodeGenState)

      case NewPair(fst, snd) =>
        newCodeGenState = compileNewPair(fst, snd, newCodeGenState)

      case Call(_, _) =>
        newCodeGenState =
          compileFunctionCall(rValueNode.asInstanceOf[Call], newCodeGenState)

      case expr: Expr =>
        newCodeGenState = compileExpression(expr, newCodeGenState)

      case _: PairElem =>
      // TODO
    }

    newCodeGenState
  }

  def compileLValue(lValueNode: LValue, codeGenState: CodeGeneratorState)(
      implicit instructions: mutable.ListBuffer[Instruction]
  ): CodeGeneratorState = {
    var newCodeGenState = codeGenState
    lValueNode match {
      case ident: Ident =>
        newCodeGenState = compileIdent(ident, newCodeGenState)
      case ArrayElem(_, _) => // TODO
      case _: PairElem     => // TODO
    }
    newCodeGenState
  }

  private def compileIdent(ident: Ident, codeGenState: CodeGeneratorState)(
      implicit instructions: mutable.ListBuffer[Instruction]
  ): CodeGeneratorState = {
    // using FP
    val offset = codeGenState.getIdentOffset(ident.name)
    instructions += Load(R1, OffsetMode(FP, shiftAmount = ImmVal(offset)))
    codeGenState
  }

  def compileNewPair(fst: Expr, snd: Expr, codeGenState: CodeGeneratorState)(
      implicit instructions: mutable.ListBuffer[Instruction]
  ): CodeGeneratorState = {
    var newCodeGenState = codeGenState
    newCodeGenState = compileExpression(fst, newCodeGenState)
    // need to add size of fst and snd somehow
    newCodeGenState = compileExpression(snd, newCodeGenState)

    instructions ++ mutable.ListBuffer(
      Load(R0, LoadImmVal(PAIR_SIZE)),
      BranchAndLink("malloc"),
      Pop(List(R2, R1)),
      Store(R1, OffsetMode(R0)),
      Store(R2, OffsetMode(R0, shiftAmount = ImmVal(WORD_SIZE)))
    )

    newCodeGenState
  }

  def compileStatWithNewScope(statNode: Stat, codeGenState: CodeGeneratorState)(
      implicit instructions: mutable.ListBuffer[Instruction]
  ): CodeGeneratorState = {
    val newCodeGenState = compileStat(statNode, codeGenState)
    newCodeGenState
  }
}
