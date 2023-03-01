package wacc.backend

import wacc.AST._
import wacc.backend.Globals.{PAIR_SIZE, WORD_SIZE}
import wacc.backend.Utils

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

    implicit val printTable: Map[(Int, Int), Type] = programNode.printTable
    programNode.funcs.foreach(func => compileFunc(func, newCodeGenState))

    instructions.addAll(
      List(
        Directive("global main"),
        Label("main"),
        Push(List(FP, LR)),
        Push(List(R4, R5, R6, R7, R8, R10, IP)),
        Move(FP, SP)
      )
    )

    newCodeGenState = newCodeGenState.copy(stackPointerOffset =
      newCodeGenState.stackPointerOffset + 4
    )

    val stackIdx = instructions.length

    // Compiles each statement in the program
    programNode.stat.foreach(stat =>
      newCodeGenState = compileStat(stat, newCodeGenState)
    )

    // add sp, sp, usedStackSize
    if (newCodeGenState.usedStackSize > 0) {
      instructions.insert(
        stackIdx,
        SubInstr(SP, SP, ImmVal(newCodeGenState.usedStackSize))
      )
      instructions.addOne(
        AddInstr(SP, SP, ImmVal(newCodeGenState.usedStackSize))
      )
    }

    // Set exit code as 0
    instructions.addOne(Move(R0, ImmVal(0)))

    instructions.addAll(
      List(
        Pop(List(R4, R5, R6, R7, R8, R10, IP)),
        Pop(List(FP, PC))
      )
    )

    println(newCodeGenState.identToReg)

    Utils.addUtils()(instructions)

    val data = Labels.dataMap
    if (data.nonEmpty) {
      Directive("text") +=: instructions
      data.flatMap(kv => kv._2.instruction) ++=: instructions
      Directive("data") +=: instructions
    }

    var curOff = -newCodeGenState.usedStackSize
    for (kv <- newCodeGenState.instructionIdxToSize) {
      kv match {
        case (idx, size) => {
          instructions.update(idx, instructions(idx).putOnStack(curOff))
          curOff += size
        }
      }
    }

    newCodeGenState.copy(stackPointerOffset =
      newCodeGenState.stackPointerOffset - 4
    )

  }

  // Compiles function declaration
  def compileFunc(funcNode: Func, codeGenState: CodeGeneratorState)(implicit
      instructions: mutable.ListBuffer[Instruction],
      printTable: Map[(Int, Int), Type]
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
    newCodeGenState =
      newCodeGenState.copy(originalSP = newCodeGenState.stackPointerOffset)

    // Compile each of the statements in the function declaration's body
    funcNode.stats.foreach(stat =>
      newCodeGenState = compileStat(stat, newCodeGenState)
    )

    newCodeGenState = newCodeGenState.copy(originalSP = 0)

    instructions.addOne(Pop(List(FP)))

    instructions.addAll(
      List(
        Pop(List(FP, PC)),
        Directive("ltorg")
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
    // TODO: Replace with codeGenState.instructionIdxToSize

    // val newIdentToOffset =
    //   codeGenState.identToOffset + (paramNode.ident -> (paramNode.ty.size + codeGenState.stackPointerOffset))
    val newUsedStackSize = codeGenState.usedStackSize + paramNode.ty.size
    val newStackPointerOffset =
      codeGenState.stackPointerOffset + paramNode.ty.size

    codeGenState.copy(
      // identToOffset = newIdentToOffset,
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
    val resReg = R0

    // Compile each of the arguments
    funcCallNode.args.foreach(arg =>
      newCodeGenState = compileExpression(arg, newCodeGenState)
    )

    instructions += BranchAndLink("wacc_" + funcCallNode.x.name)

    if (argsSize > 0) {
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

    newCodeGenState.copy(
      stackPointerOffset = newCodeGenState.stackPointerOffset - argsSize
    )
  }

  // Compiles expression
  def compileExpression(exprNode: Expr, codeGenState: CodeGeneratorState)(
      implicit instructions: mutable.ListBuffer[Instruction]
  ): CodeGeneratorState = {
    var newCodeGenState = codeGenState
    val resReg = newCodeGenState.tmp
    val operand1Reg = newCodeGenState.fallbackReg

    exprNode match {
      case ident: Ident =>
        newCodeGenState = compileIdent(ident, newCodeGenState)

      // Case for binary expressions
      case Add(_, _) | Sub(_, _) | Mult(_, _) | Div(_, _) | Mod(_, _) |
          And(_, _) | Or(_, _) | LT(_, _) | LTE(_, _) | GT(_, _) | GTE(_, _) |
          Equal(_, _) | NotEqual(_, _) => {
        val operand2Reg = newCodeGenState.fallbackReg2
        // newCodeGenState.getScratchReg match {
        //   case Some(reg) => reg
        //   case None      => {
        //                   instructions.addOne(Push(List(newCodeGenState.fallbackReg2)))
        //                   newCodeGenState.fallbackReg2
        //               }
        // }

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
        instructions.addOne(Pop(List(newCodeGenState.fallbackReg)))
      }

      // Case for unary expressions
      case Not(_) | Neg(_) | Len(_) | Ord(_) | Chr(_) | Ident(_) => {
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
            instructions += Move(codeGenState.tmp, ImmVal(x))

          case BoolLiter(x) =>
            instructions += Move(codeGenState.tmp, ImmVal(if (x) 1 else 0))

          case CharLiter(x) =>
            instructions += Move(codeGenState.tmp, ImmChar(x))

          case StrLiter(x) =>
            instructions += Load(
              codeGenState.tmp,
              LabelOp(Labels.addDataMsg(x))
            )

          case Null() =>
            instructions += Move(codeGenState.tmp, ImmVal(0))

          case _ => ()
        }
      }

      case _ => ()
    }

    newCodeGenState
  }

  def compileStat(statNode: Stat, codeGenState: CodeGeneratorState)(implicit
      instructions: mutable.ListBuffer[Instruction],
      printTable: Map[(Int, Int), Type]
  ): CodeGeneratorState = {
    var newCodeGenState = codeGenState

    statNode match {
      case Skip() =>
        newCodeGenState

      case Assign(lValue, rValue) => {
        newCodeGenState = compileRValue(rValue, newCodeGenState)
        newCodeGenState = compileLValue(lValue, newCodeGenState)
        
        instructions += Store(
          newCodeGenState.getScratchReg.get,
          OffsetMode(newCodeGenState.getScratchReg.get) //str r7
        )
      }

      case Declare(ty, x, y) =>
        {
          newCodeGenState = compileRValue(y, newCodeGenState)

          newCodeGenState.getScratchReg match {
            case Some(reg) => {
              instructions += Move(reg, newCodeGenState.tmp)
              newCodeGenState = newCodeGenState.copy(identToReg =
                newCodeGenState.identToReg + (x -> reg)
              )
            }
            case None => {
              // TODO: verify that this is correct
              instructions += Store(
                newCodeGenState.tmp,
                OffsetMode(FP, shiftAmount = ImmVal(ty.size))
              )

              newCodeGenState = newCodeGenState.copy(
                usedStackSize = newCodeGenState.usedStackSize + ty.size,
                instructionIdxToSize = newCodeGenState.instructionIdxToSize
                  .addOne(instructions.length, ty.size)
              )
            }
          }
        }

      case Read(_) =>
      // TODO

      case Free(_) =>
      // TODO

      case Return(expr) =>
        val resReg = R0
        newCodeGenState = compileExpression(expr, newCodeGenState)

        if (resReg != R0)
          instructions += Move(R0, resReg)

        val stackPointerOffsetDiff =
          newCodeGenState.stackPointerOffset - newCodeGenState.originalSP
        if (stackPointerOffsetDiff > 0) {
          instructions += AddInstr(
            SP,
            SP,
            ImmVal(stackPointerOffsetDiff)
          )
        }

        newCodeGenState = newCodeGenState.copy(
          stackPointerOffset = newCodeGenState.originalSP
        )

      case Exit(expr) =>
        newCodeGenState = compileExpression(expr, newCodeGenState)
        instructions += Move(R0, newCodeGenState.tmp)
        instructions += BranchAndLink("exit")

        newCodeGenState = newCodeGenState.copy()

      case Print(expr) =>
        newCodeGenState = compileExpression(expr, newCodeGenState)
        instructions += Move(R0, newCodeGenState.tmp)
        printTable.get(expr.pos) match {
          case Some(IntType()) =>
            if (!Utils.printIntFlag) {
              Utils.printIntFlag = true
              Labels.addDataMsg("%d\u0000")
            }
            instructions += BranchAndLink("_printi")
          case Some(CharType()) =>
            if (!Utils.printCharFlag)
              Utils.printCharFlag = true
            instructions += BranchAndLink("_printc")
          case Some(StringType()) | Some(ArrayType(CharType())) =>
            if (!Utils.printStringFlag)
              Utils.printStringFlag = true
            instructions += BranchAndLink("_prints")
          case Some(BoolType()) =>
            if (!Utils.printBoolFlag)
              Utils.printBoolFlag = true
            instructions += BranchAndLink("_printb")
          case _ => ()
        }

      case Println(expr) =>
        newCodeGenState = compileExpression(expr, newCodeGenState)
        instructions += Move(R0, newCodeGenState.tmp)
        printTable.get(expr.pos) match {
          case Some(IntType()) =>
            if (!Utils.printIntFlag) {
              Utils.printIntFlag = true
              Labels.addDataMsg("%d\u0000")
            }
            instructions += BranchAndLink("_printi")
          case Some(CharType()) =>
            if (!Utils.printCharFlag) {
              Utils.printCharFlag = true
              instructions += BranchAndLink("_printc")
            }
          case Some(StringType()) | Some(ArrayType(CharType())) =>
            if (!Utils.printStringFlag)
              Utils.printStringFlag = true
            instructions += BranchAndLink("_prints")
          case Some(BoolType()) =>
            if (!Utils.printBoolFlag) {
              Utils.printBoolFlag = true
              Labels.addDataMsg("false\u0000")
              Labels.addDataMsg("true\u0000")
            }
            instructions += BranchAndLink("_printb")
          case _ => ()
        }
        Utils.printlnFlag = true
        instructions += BranchAndLink("_println")

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

        val condReg = newCodeGenState.getScratchReg.get

        instructions += Label(condLabel)
        newCodeGenState = compileExpression(cond, newCodeGenState)

        instructions.addAll(
          List(
            Cmp(condReg, ImmVal(1)),
            Branch(bodyLabel, Condition.EQ)
          )
        )

      case Scope(stats) =>
        stats.foreach(stat =>
          newCodeGenState = compileStatWithNewScope(stat, newCodeGenState)
        ) // TODO - implement compileBlock or similar
    }

    newCodeGenState
  }

  // Compiles 'if-then-else' statements
  def compileIfStat(ifNode: If, codeGenState: CodeGeneratorState)(implicit
      instructions: mutable.ListBuffer[Instruction],
      printTable: Map[(Int, Int), Type]
  ): CodeGeneratorState = {
    val condReg = codeGenState.getScratchReg.get

    // Compile condition
    var newCodeGenState = compileExpression(ifNode.cond, codeGenState)

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
         if (xs.length != 0) {
          val arrayItemSize = xs.head.size
          val arraySize = xs.head.size * xs.length
          val tmp = R8
          val arrayStartReg: Register = newCodeGenState.getScratchReg.get

          instructions.addAll(
            List(
              Move(R0, ImmVal(WORD_SIZE + arraySize)),
              BranchAndLink("malloc"),
              Move(arrayStartReg, R0),
              AddInstr(arrayStartReg, arrayStartReg, ImmVal(WORD_SIZE)),
              Move(tmp, ImmVal(xs.length)),
              Store(tmp, OffsetMode(arrayStartReg, shiftAmount = ImmVal(-4)))
            )
          )

          for ((expr, i) <- xs.zipWithIndex) {
            newCodeGenState = compileExpression(expr, newCodeGenState)
            instructions += Store(
              tmp,
              OffsetMode(arrayStartReg, shiftAmount = ImmVal(arrayItemSize * i))
            )
          }

          instructions.addAll(
            List(
              Move(tmp, arrayStartReg),
              Move(IP, tmp)
            )
          )
        }


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
      case ArrayElem(ident: Ident, xs: List[Expr]) =>
        instructions.addAll(
          List(
            // R3 contains array pointer - THIS NEEDS TO BE CHECKED
            Move(R3, ImmVal(0)),   // ImmVal(newCodeGenState.getIdentOffset(ident.name))),
            // R7 contains rValue to be assigned
            Move(R7, R8)
          )
        )
        
        val indexReg = newCodeGenState.getScratchReg.get
        newCodeGenState = compileExpression(xs.head, newCodeGenState)
        // R10 contains index value

        instructions.addAll(
          List(
            Move(R10, indexReg),
            BranchAndLink("_arrStore")
          )
        )


        instructions.addAll(
          List(
            /* @ Special calling convention: array ptr passed in R3, index in R10, 
               value to store in R7, LR (R14) is used as general register */
            Label("_arrStore"),
            Push(List(LR)),
            Cmp(R10, ImmVal(0)),
            Move(R1, R10, Condition.LT),
            BranchAndLink("_boundsCheck", Condition.LT),
            Load(LR, OffsetMode(baseReg = R3, shiftAmount = ImmVal(-4))),
            Cmp(R10, LR),
            Move(R1, R10, Condition.GE),
            BranchAndLink("_boundsCheck", Condition.GE),
            Store(newCodeGenState.tmp, OffsetMode(baseReg = R3, auxReg = Some(R10), shiftType = Some(ShiftType.LSL), shiftAmount = ImmVal(2)))
          )
        )

      case _: PairElem     => // TODO
    }
    newCodeGenState
  }

  private def compileIdent(ident: Ident, codeGenState: CodeGeneratorState)(
      implicit instructions: mutable.ListBuffer[Instruction]
  ): CodeGeneratorState = {
    codeGenState.identToReg.get(ident) match {
      case Some(reg) =>
        instructions ++= List(
          Move(codeGenState.tmp, reg)
        )
      case None =>
        // val offset = codeGenState.getIdentOffset(ident)
        instructions += Load(
          codeGenState.tmp,
          OffsetMode(FP, shiftAmount = ImmVal(4))
        )
    }
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
      implicit
      instructions: mutable.ListBuffer[Instruction],
      printTable: Map[(Int, Int), Type]
  ): CodeGeneratorState = {
    val newCodeGenState = compileStat(statNode, codeGenState)
    newCodeGenState
  }
}
