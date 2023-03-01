package wacc.backend

import wacc.AST._
import wacc.backend.Globals.{PAIR_SIZE, WORD_SIZE}
import wacc.backend.Utils
import scala.collection.mutable

object CodeGenerator {
  def compileProgram(
      programNode: Program
  )(implicit state: CodeGenState): mutable.ListBuffer[Instruction] = {
    val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer.empty

    programNode.funcs.foreach(func => {
      implicit val printTable: Map[(Int, Int), Type] = func.printTable
      implicit val symbolTable: Map[Ident, Type] = func.symbolTable
      implicit val funcLabels = new Labels(func.ident.name)
      instructions ++= compileFunc(func)
    })

    implicit val printTable: Map[(Int, Int), Type] = programNode.printTable
    implicit val symbolTable: Map[Ident, Type] = programNode.symbolTable
    implicit val mainLabels = new Labels("main")

    // instructions ++= List(
    //   Label("main")
    //   Push(List(FP, LR)),
    //   Push(List(R4, R5, R6, R7, R8, R10, IP)),
    //   Move(FP, SP)
    // )

    instructions ++= StackMachine.addStackFrame(programNode.symbolTable)

    instructions ++= compileStats(programNode.stat)

    // instructions ++= List(
    //   Move(R0, ImmVal(0)), // Set exit code as zero
    //   Pop(List(R4, R5, R6, R7, R8, R10, IP)),
    //   Pop(List(FP, PC))
    // )

    instructions += Move(R0, ImmVal(0))
    instructions ++= StackMachine.removeStackFrame()
    instructions += Pop(List(PC))

    Utils.addUtils()(instructions)
    mainLabels.addLabelInstructions(instructions)

    instructions
  }

  def compileFunc(funcNode: Func)(implicit
      state: CodeGenState,
      printTable: Map[(Int, Int), Type],
      symbolTable: Map[Ident, Type],
      labels: Labels
  ): mutable.ListBuffer[Instruction] = mutable.ListBuffer.empty

  def compileStats(stats: List[Stat])(implicit
      state: CodeGenState,
      printTable: Map[(Int, Int), Type],
      symbolTable: Map[Ident, Type],
      labels: Labels
  ): mutable.ListBuffer[Instruction] = {
    val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer.empty

    stats.foreach(stat => {
      instructions ++= compileStat(stat)
      println(s"stat=$stat, identToReg=${state.identToReg}")
    })

    instructions
  }

  def compileStat(stat: Stat)(implicit
      state: CodeGenState,
      printTable: Map[(Int, Int), Type],
      symbolTable: Map[Ident, Type],
      labels: Labels
  ): mutable.ListBuffer[Instruction] = {
    val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer.empty
    stat match {
      case Skip() => instructions

      case Declare(ty, ident, rValue) => {
        state.getScratchReg match {
          case Some(scratchReg) => {
            instructions ++= compileRValue(rValue, scratchReg)
            state.identToReg += (ident -> scratchReg)
          }
          case None => {
            StackMachine.addDeclaration(ident, ty.size)
            instructions ++= compileRValue(rValue, state.tmp)
            instructions += storeRes(
              StackMachine.getIdentOffset(ident),
              ty.size,
              state.tmp
            )
          }
        }

        // StackMachine.addDeclaration(ident, ty.size)
        // instructions ++= compileRValue(rValue, state.tmp)
        // instructions += storeRes(
        //   StackMachine.getIdentOffset(ident),
        //   ty.size,
        //   state.tmp
        // )
      }

      case Assign(lValue, rValue) => {
        instructions += Comment(s"ASSIGN $lValue = $rValue, LHS")
        instructions ++= compileRValue(rValue, state.tmp2)
        instructions += Comment(s"ASSIGN $lValue = $rValue, RHS")
        instructions ++= compileLValue(lValue, state.tmp)

        instructions += Comment(s"ASSIGN MOVE")
        instructions += Move(state.tmp, state.tmp2)

        // instructions += Store(
        //    state.getScratchReg.get,
        //    OffsetMode(state.getScratchReg.get))
      }

      case Read(_) => () // TODO

      case Free(_) => () // TODO

      case Return(expr) => instructions ++= compileExpr(expr, R0)

      case Exit(expr) => {
        instructions ++= compileExpr(expr, R0)
        instructions += BranchAndLink("exit")
      }

      case Print(expr) => {
        instructions ++= compileExpr(expr, R0)
        printTable.get(expr.pos) match {
          case Some(IntType()) =>
            if (!Utils.printIntFlag) {
              Utils.printIntFlag = true
              labels.addDataMsg("%d\u0000")
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
          case _ => mutable.ListBuffer.empty
        }
      }

      case Println(expr) => {
        instructions += Comment(s"PRINTLN $expr")
        instructions ++= compileExpr(expr, R0)
        printTable.get(expr.pos) match {
          case Some(IntType()) =>
            if (!Utils.printIntFlag) {
              Utils.printIntFlag = true
              labels.addDataMsg("%d\u0000")
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
              labels.addDataMsg("false\u0000")
              labels.addDataMsg("true\u0000")
            }
            instructions += BranchAndLink("_printb")
          case _ => ()
        }
        Utils.printlnFlag = true
        instructions += BranchAndLink("_println")
      }

      case ifStatNode @ If(_, _, _) =>
        instructions ++= compileIfStat(ifStatNode, state.tmp)

      case whileNode @ While(cond, bodyStat) => {
        val uniqueWhileName = "while_" + state.getNewLabelId;
        val condLabel = uniqueWhileName + "_cond"
        val bodyLabel = uniqueWhileName + "_body"
        instructions += Branch(condLabel)

        instructions += Label(bodyLabel)
        StackMachine.addStackFrame(whileNode.symbolTable)
        bodyStat.foreach(stat => instructions ++= compileStat(stat))

        val condReg = state.getScratchReg.get

        instructions += Label(condLabel)
        instructions ++= compileExpr(cond, state.tmp)

        instructions.addAll(
          List(
            Cmp(condReg, ImmVal(1)),
            Branch(bodyLabel, Condition.EQ)
          )
        )
      }

      case scopeNode @ Scope(stats) => {
        StackMachine.addStackFrame(scopeNode.symbolTable)
        stats.foreach(stat => instructions ++= compileStat(stat))
        // TODO - implement compileBlock or similar
      }
    }
    instructions
  }

  def compileRValue(rValue: RValue, resReg: Register)(implicit
      state: CodeGenState,
      printTable: Map[(Int, Int), Type],
      symbolTable: Map[Ident, Type],
      labels: Labels
  ): mutable.ListBuffer[Instruction] = {
    val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer.empty

    rValue match {
      // case ArrayLit(xs) => {
      //   if (xs.length != 0) {
      //     val arrayItemSize = xs.head.size
      //     val arraySize = xs.head.size * xs.length
      //   }
      // }

      case ArrayLit(xs) =>
        if (xs.length != 0) {
          val arrayItemSize = xs.head.size
          val arraySize = xs.head.size * xs.length
          val arrayStartReg: Register = state.getScratchReg.get

          instructions.addAll(
            List(
              Move(R0, ImmVal(WORD_SIZE + arraySize)),
              BranchAndLink("malloc"),
              Move(arrayStartReg, R0),
              AddInstr(arrayStartReg, arrayStartReg, ImmVal(WORD_SIZE)),
              Move(state.tmp, ImmVal(xs.length)),
              Store(
                state.tmp,
                OffsetMode(arrayStartReg, shiftAmount = ImmVal(-4))
              )
            )
          )

          for ((expr, i) <- xs.zipWithIndex) {
            instructions ++= compileExpr(expr, state.tmp)
            instructions += Store(
              state.tmp,
              OffsetMode(arrayStartReg, shiftAmount = ImmVal(arrayItemSize * i))
            )
          }

          instructions += Move(resReg, arrayStartReg)
        }

      case NewPair(fst, snd) => instructions ++= compileNewPair(fst, snd)
      case Call(_, _) =>
        instructions ++= compileFunctionCall(rValue.asInstanceOf[Call])
      case expr: Expr  => instructions ++= compileExpr(expr, resReg)
      case _: PairElem =>
    }

    instructions
  }

  def compileLValue(lValue: LValue, resReg: Register)(implicit
      state: CodeGenState,
      printTable: Map[(Int, Int), Type],
      symbolTable: Map[Ident, Type],
      labels: Labels
  ): mutable.ListBuffer[Instruction] = {
    val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer.empty

    lValue match {
      case ident: Ident => instructions ++= compileIdent(ident, resReg)
      case ArrayElem(ident: Ident, xs: List[Expr]) =>
        instructions.addAll(
          List(
            // R3 contains array pointer - THIS NEEDS TO BE CHECKED
            Move(
              R3,
              ImmVal(0)
            ), // ImmVal(newCodeGenState.getIdentOffset(ident.name))),
            // R7 contains rValue to be assigned
            Move(R7, R8)
          )
        )

        val indexReg = state.getScratchReg.get
        instructions ++= compileExpr(xs.head, indexReg)
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
            Store(
              state.tmp,
              OffsetMode(
                baseReg = R3,
                auxReg = Some(R10),
                shiftType = Some(ShiftType.LSL),
                shiftAmount = ImmVal(2)
              )
            )
          )
        )

      case _: PairElem => // TODO
    }
    instructions
  }

  def compileNewPair(fst: Expr, snd: Expr)(implicit
      state: CodeGenState,
      printTable: Map[(Int, Int), Type],
      symbolTable: Map[Ident, Type],
      labels: Labels
  ): mutable.ListBuffer[Instruction] = mutable.ListBuffer.empty

  def compileFunctionCall(funcCallNode: Call)(implicit
      state: CodeGenState,
      printTable: Map[(Int, Int), Type],
      symbolTable: Map[Ident, Type],
      labels: Labels
  ): mutable.ListBuffer[Instruction] = mutable.ListBuffer.empty

  def compileExpr(expr: Expr, resReg: Register)(implicit
      state: CodeGenState,
      printTable: Map[(Int, Int), Type],
      symbolTable: Map[Ident, Type],
      labels: Labels
  ): mutable.ListBuffer[Instruction] = {
    val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer.empty
    val operand1Reg = state.tmp
    val operand2Reg = state.tmp2

    expr match {
      case ident: Ident => instructions ++= compileIdent(ident, resReg)
      // TODO: Overflow
      case Mult(x, y) =>
        instructions ++= compileExpr(x, operand1Reg)
        instructions ++= compileExpr(y, operand2Reg)
        instructions += SMull(
          operand1Reg,
          operand2Reg,
          operand1Reg,
          operand2Reg
        )

      // TODO
      case Div(x, y) =>
        instructions ++= compileExpr(x, operand1Reg)
        instructions ++= compileExpr(y, operand2Reg)
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
        instructions ++= compileExpr(x, operand1Reg)
        instructions ++= compileExpr(y, operand2Reg)
        instructions.addAll(
          List(
            BranchAndLink(
              "__aeabi_idivmod"
            ), // signed __aeabi_idivmod(signed numerator, signed denominator)
            Move(resReg, R1)
          )
        )

      case Add(x, y) =>
        instructions ++= compileExpr(x, operand1Reg)
        instructions ++= compileExpr(y, operand2Reg)
        instructions += AddInstr(resReg, operand1Reg, operand2Reg)

      case And(x, y) =>
        instructions ++= compileExpr(x, operand1Reg)
        instructions ++= compileExpr(y, operand2Reg)
        instructions += AndInstr(resReg, operand1Reg, operand2Reg)

      case Or(x, y) =>
        instructions ++= compileExpr(x, operand1Reg)
        instructions ++= compileExpr(y, operand2Reg)
        instructions += OrrInstr(resReg, operand1Reg, operand2Reg)

      case Sub(x, y) =>
        instructions ++= compileExpr(x, operand1Reg)
        instructions ++= compileExpr(y, operand2Reg)
        instructions += SubInstr(resReg, operand1Reg, operand2Reg)

      case LT(x, y) =>
        instructions ++= compileExpr(x, operand1Reg)
        instructions ++= compileExpr(y, operand2Reg)
        instructions.addAll(
          List(
            Cmp(operand1Reg, operand2Reg),
            Move(resReg, ImmVal(1), Condition.LT),
            Move(resReg, ImmVal(0), Condition.GE)
          )
        )

      case LTE(x, y) =>
        instructions ++= compileExpr(x, operand1Reg)
        instructions ++= compileExpr(y, operand2Reg)
        instructions.addAll(
          List(
            Cmp(operand1Reg, operand2Reg),
            Move(resReg, ImmVal(1), Condition.LE),
            Move(resReg, ImmVal(0), Condition.GT)
          )
        )

      case GT(x, y) =>
        instructions ++= compileExpr(x, operand1Reg)
        instructions ++= compileExpr(y, operand2Reg)
        instructions.addAll(
          List(
            Cmp(operand1Reg, operand2Reg),
            Move(resReg, ImmVal(1), Condition.GT),
            Move(resReg, ImmVal(0), Condition.LE)
          )
        )

      case GTE(x, y) =>
        instructions ++= compileExpr(x, operand1Reg)
        instructions ++= compileExpr(y, operand2Reg)
        instructions.addAll(
          List(
            Cmp(operand1Reg, operand2Reg),
            Move(resReg, ImmVal(1), Condition.GE),
            Move(resReg, ImmVal(0), Condition.LT)
          )
        )

      case NotEqual(x, y) =>
        instructions ++= compileExpr(x, operand1Reg)
        instructions ++= compileExpr(y, operand2Reg)
        instructions.addAll(
          List(
            Cmp(operand1Reg, operand2Reg),
            Move(resReg, ImmVal(1), Condition.NE),
            Move(resReg, ImmVal(0), Condition.EQ)
          )
        )

      case Equal(x, y) =>
        instructions ++= compileExpr(x, operand1Reg)
        instructions ++= compileExpr(y, operand2Reg)
        instructions.addAll(
          List(
            Cmp(operand1Reg, operand2Reg),
            // Move(R7, ImmVal(1), Condition.EQ),
            // Move(R7, ImmVal(0), Condition.NE)
            Move(resReg, ImmVal(1), Condition.EQ),
            Move(resReg, ImmVal(0), Condition.NE)
          )
        )
      // cmp r4, r5,		moveq r7, #1,  movne r7, #0

      case Not(x) => {
        instructions ++= compileExpr(x, resReg)
        instructions += XorInstr(resReg, resReg, ImmVal(1))
      }

      case Neg(x) => {
        instructions ++= compileExpr(x, resReg)
        instructions += Rsb(resReg, resReg, ImmVal(0))
      }

      case Len(x) => {
        instructions ++= compileExpr(x, resReg)
        instructions += Load(resReg, OffsetMode(resReg))
      }
      case Ord(x) => instructions ++= compileExpr(x, resReg)

      case Chr(x) => instructions ++= compileExpr(x, resReg)

      case IntegerLiter(x) =>
        instructions += Move(resReg, ImmVal(x))

      case BoolLiter(x) => {
        instructions += Move(resReg, ImmVal(if (x) 1 else 0))
      }

      case CharLiter(x) => {
        instructions += Move(resReg, ImmChar(x))
      }
      case StrLiter(x) => {
        instructions += Load(
          resReg,
          LabelOp(labels.addDataMsg(x))
        )
      }
      case Null() =>
        instructions += Move(state.tmp, ImmVal(0))
      case ArrayElem(ident: Ident, xs: List[Expr]) =>
        instructions.addAll(
          List(
            // R3 contains array pointer - THIS NEEDS TO BE CHECKED
            Move(
              R3,
              ImmVal(0)
            ), // ImmVal(newCodeGenState.getIdentOffset(ident.name))),
            // R7 contains rValue to be assigned
            Move(R7, R8)
          )
        )

        val indexReg = state.getScratchReg.get // TODO: Sort out
        instructions ++= compileExpr(xs.head, indexReg)
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
            Store(
              state.tmp,
              OffsetMode(
                baseReg = R3,
                auxReg = Some(R10),
                shiftType = Some(ShiftType.LSL),
                shiftAmount = ImmVal(2)
              )
            )
          )
        )

      case Bracket(x) => instructions ++= compileExpr(x, resReg)
    }
    instructions
  }

  def compileIfStat(ifNode: If, resReg: Register)(implicit
      state: CodeGenState,
      printTable: Map[(Int, Int), Type],
      symbolTable: Map[Ident, Type],
      labels: Labels
  ): mutable.ListBuffer[Instruction] = {
    val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer.empty

    val condReg = state.tmp

    // Compile condition
    instructions ++= compileExpr(ifNode.cond, condReg)

    val thenLabel = "l_" + state.getNewLabelId
    val endLabel = "l_" + state.getNewLabelId

    instructions.addAll(
      List(
        Cmp(condReg, ImmVal(1)),
        Branch(thenLabel, Condition.EQ)
      )
    )

    // Compile else statement
    // instructions ++= StackMachine.addStackFrame(ifNode.elseSymbolTable)
    ifNode.elseStat.foreach(stat =>
      instructions ++= compileStat(stat)(
        state,
        printTable,
        ifNode.elseSymbolTable,
        labels
      )
    )
    // instructions ++= StackMachine.removeStackFrame()

    instructions.addAll(
      List(
        Branch(endLabel),
        Label(thenLabel)
      )
    )

    // Compile then statement
    // instructions ++= StackMachine.addStackFrame(ifNode.thenSymbolTable)
    ifNode.thenStat.foreach(stat =>
      instructions ++= compileStat(stat)(
        state,
        printTable,
        ifNode.thenSymbolTable,
        labels
      )
    )
    // instructions ++= StackMachine.removeStackFrame()

    instructions += Label(endLabel)

    instructions

  }

  def compileIdent(ident: Ident, resReg: Register)(implicit
      state: CodeGenState,
      printTable: Map[(Int, Int), Type],
      symbolTable: Map[Ident, Type],
      labels: Labels
  ): mutable.ListBuffer[Instruction] = {
    val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer.empty
    state.identToReg.get(ident) match {
      case Some(reg) =>
        if (reg != resReg) {
          instructions ++= List(
            Move(resReg, reg)
          )
          state.identToReg += (ident -> resReg)
        }
      case None =>
        instructions += Load(
          state.tmp,
          OffsetMode(
            FP,
            shiftAmount = ImmVal(StackMachine.getIdentOffset(ident))
          )
        )
    }
    instructions
  }

  /*
    private def compileBinOp(x: Expr, y: Expr)(implicit state: state, printTable: Map[(Int, Int), Type], symbolTable: Map[Ident, Type], labels: Labels): mutable.ListBuffer[Instruction] = {

      val recentReg = state.recentReg match {
        case Some(reg) => reg
        case _  =>
      }
      val secondRecentReg = state.secondRecentReg match {
        case Some(reg) => reg
        case _  =>
      }
      compileExpr(x, recentReg) ++ compileExpr(y, secondRecentReg)


    } */

  def storeRes(offset: Int, size: Int, resReg: Register): Instruction = {
    size match {
      case 1 => StoreByte(resReg, OffsetMode(FP, shiftAmount = ImmVal(offset)))
      case _ => Store(resReg, OffsetMode(FP, shiftAmount = ImmVal(offset)))
    }
  }

}
