package wacc.backend

import wacc.AST._
import wacc.backend.Globals.WORD_SIZE
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
    // implicit val functionTable: Map[Ident, (Type, List[Type])] = programNode.functionTable
    implicit val mainLabels = new Labels("main")

    instructions ++= StackMachine.addStackFrame(programNode.symbolTable)

    instructions ++= compileStats(programNode.stat)

    instructions.mapInPlace(instr =>
      instr match {
        case PendingStackOffset(instr) =>
          instr.putOnStack(StackMachine.currStackSize)
        case _ => instr
      }
    )
    instructions += Move(R0, ImmVal(0))

    println(state.identToReg)
    println(StackMachine.stackFrameList.last.declaredVarMap)

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
  ): mutable.ListBuffer[Instruction] = {

    val funcSymbolTable = funcNode.symbolTable
    val funcPrintTable = funcNode.printTable
    val funcName: String = "wacc_" + funcNode.ident.name
    val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer.empty

    // instructions.addAll(
    //   List(
    //     Label(funcName),
    //     Push(List(LR))

    //   )
    // )

    mutable.ListBuffer.empty
  }

  def compileStats(stats: List[Stat])(implicit
      state: CodeGenState,
      printTable: Map[(Int, Int), Type],
      symbolTable: Map[Ident, Type],
      labels: Labels
  ): mutable.ListBuffer[Instruction] = {
    val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer.empty

    stats.foreach(instructions ++= compileStat(_))

    instructions.mapInPlace(instr =>
      instr match {
        case PendingStackOffset(instr) =>
          instr.putOnStack(StackMachine.currStackSize)
        case _ => instr
      }
    )

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
        state.getRegOrNone match {
          case Some(scratchReg) => {
            instructions += Comment(
              "Declare Some(scratchReg) case, scratchReg = " + scratchReg
            )
            instructions ++= compileRValue(rValue, scratchReg)
            state.identToReg += (ident -> scratchReg)
          }
          case None => {
            val offset = StackMachine.addDeclaration(ident, ty.size)
            instructions ++= compileRValue(rValue, state.tmp)
            instructions += PendingStackOffset(
              storeRes(offset, ty.size, state.tmp)
            )
          }
        }
      }

      case Assign(lValue, rValue) => {
        instructions ++= compileRValue(rValue, state.tmp)

        lValue match {
          case ident: Ident => {
            state.identToReg.get(ident) match {
              case Some(reg) => {
                instructions += Move(reg, state.tmp)
              }
              case None => {
                val offset = StackMachine.getIdentOffset(ident)
                instructions += PendingStackOffset(
                  storeRes(offset, ident.size, state.tmp)
                )
              }
            }
          }
          case _ => {
            instructions ++= compileLValue(lValue, state.tmp2)
            instructions += Store(state.tmp, OffsetMode(state.tmp2))
          }
        }
      }

      case Read(lValue: LValue) => {
        instructions ++= compileLValue(lValue, state.tmp)
        instructions += Move(R0, state.tmp)
        printTable.get(lValue.pos) match {
          case Some(IntType()) => {
            if (!Utils.readIntFlag) {
              Utils.readIntFlag = true
              labels.addDataMsg("%d\u0000")
            }
            instructions += BranchAndLink("_readi")
          }

          case Some(CharType()) => {
            if (!Utils.readCharFlag) {
              Utils.readCharFlag = true
              labels.addDataMsg(" %c\u0000")
            }
            instructions += BranchAndLink("_readc")
          }

          case _ => //
        }

        lValue match {
          case ident: Ident => {
            state.identToReg.get(ident) match {
              case Some(reg) => {
                instructions += Move(reg, R0)
              }
              case None => {
                val offset = StackMachine.getIdentOffset(ident)
                instructions += PendingStackOffset(
                  storeRes(offset, ident.size, R0)
                )
              }
            }
          }
          case _ => {
            instructions ++= compileLValue(lValue, state.tmp2)
            instructions += Store(R0, OffsetMode(state.tmp2))
          }
        }
      }

      case Free(expr) => {}

      case Return(expr) => {
        instructions ++= compileExpr(expr, state.tmp)
        instructions += Move(R0, state.tmp)
      }
      case Exit(expr) => {
        instructions ++= compileExpr(expr, state.tmp)
        instructions += Move(R0, state.tmp)
        instructions += BranchAndLink("exit")
      }

      case Print(expr) => {
        println("PRINT TABLE: " + printTable)
        instructions ++= compileExpr(expr, state.tmp)
        instructions += Move(R0, state.tmp)
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

          // int[] a = [1, 2]
          // print(a) -> reference to array
          // case Some() =>

          case _ =>
            if (!Utils.printPFlag) {
              Utils.printPFlag = true
              labels.addDataMsg("%p\u0000")
            }
            instructions += BranchAndLink("_printp")
        }
      }

      case Println(expr) => {
        println("PRINT TABLE: " + printTable)
        instructions ++= compileExpr(expr, state.tmp)
        instructions += Move(R0, state.tmp)
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
            }
            instructions += BranchAndLink("_printc")
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
          case _ =>
            if (!Utils.printPFlag) {
              Utils.printPFlag = true
              labels.addDataMsg("%p\u0000")
            }
            instructions += BranchAndLink("_printp")
        }
        Utils.printlnFlag = true
        instructions += BranchAndLink("_println")
      }

      case ifStatNode @ If(_, _, _) =>
        instructions ++= compileIfStat(
          ifStatNode,
          state.getReg
        ) // (ifStatNode, state.tmp)

      case whileNode @ While(cond, bodyStat) => {
        val uniqueWhileName = "while_" + state.getNewLabelId;
        val condLabel = uniqueWhileName + "_cond"
        val bodyLabel = uniqueWhileName + "_body"
        instructions += Branch(condLabel)

        instructions += Label(bodyLabel)
        StackMachine.addStackFrame(whileNode.symbolTable)
        bodyStat.foreach(stat => instructions ++= compileStat(stat))

        val condReg = state.getReg

        instructions += Label(condLabel)
        instructions ++= compileExpr(cond, condReg)

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
      case ArrayLit(xs) =>
        if (xs.length != 0) {
          Utils.arrayFlag = true
          val arrayItemSize = xs.head.size
          val arraySize = arrayItemSize * xs.length
          val arrayStartReg: Register = state.tmp3
          val a_reg = state.tmp

          instructions.addAll(
            List(
              Move(R0, ImmVal(WORD_SIZE + arraySize)),
              BranchAndLink("malloc"),
              Move(arrayStartReg, R0),
              AddInstr(arrayStartReg, arrayStartReg, ImmVal(WORD_SIZE)),
              Move(a_reg, ImmVal(xs.length)),
              Store(
                a_reg,
                OffsetMode(arrayStartReg, shiftAmount = ImmVal(-4))
              )
            )
          )

          for ((expr, i) <- xs.zipWithIndex) {
            instructions ++= compileExpr(expr, a_reg)
            instructions += Store(
              a_reg,
              OffsetMode(arrayStartReg, shiftAmount = ImmVal(arrayItemSize * i))
            )
          }

          instructions += Move(resReg, arrayStartReg)
        }

      case NewPair(expr1, expr2) =>
        instructions ++= compileNewPair(expr1, expr2)
      case Call(_, _) =>
        instructions ++= compileFunctionCall(rValue.asInstanceOf[Call])
      case expr: Expr     => instructions ++= compileExpr(expr, resReg)
      case pair: PairElem =>
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
        Utils.arrayFlag = true

        // R10 contains index value
        /*xs.foreach(e: Expr => {



        }) */

        instructions ++= compileExpr(xs.head, R10) // TODO: multidimensional

        instructions += Push(List(R3))

        state.identToReg.get(ident) match {
          case Some(reg) => instructions += Move(R3, reg)
          case None =>
            instructions += Move(
              R3,
              ImmVal(StackMachine.getIdentOffset(ident))
            ) // TODO: PendingStackOffset?
        }

        instructions ++= List(
          BranchAndLink("_arrStore"),
          Move(resReg, R3),
          Pop(List(R3))
        )

      case _: PairElem => // TODO
    }
    instructions
  }

  def compileNewPair(expr1: Expr, expr2: Expr)(implicit
      state: CodeGenState,
      printTable: Map[(Int, Int), Type],
      symbolTable: Map[Ident, Type],
      labels: Labels
  ): mutable.ListBuffer[Instruction] = {
    mutable.ListBuffer.empty

    // val sizeExpr1 = expr1 match
  }

  def compileFunctionCall(funcCallNode: Call)
  // (implicit
  //     state: CodeGenState,
  //     printTable: Map[(Int, Int), Type],
  //     symbolTable: Map[Ident, Type],
  //     labels: Labels)
      : mutable.ListBuffer[Instruction] = mutable.ListBuffer.empty

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
      case ident: Ident =>
        instructions ++= compileIdent(ident, resReg)
      case Mult(x, y) => {
        Utils.intErrOverflowFlag = true

        instructions ++= compileExpr(x, operand1Reg)
        instructions += Push(List(operand1Reg))
        instructions ++= compileExpr(y, operand2Reg)
        instructions += Pop(List(operand1Reg))

        val rdHi = state.tmp3
        instructions += SMull(
          resReg, // RdLo
          rdHi, // RdHi
          operand1Reg,
          operand2Reg
        )

        instructions ++= List(
          CmpShift(
            rdHi,
            resReg,
            ShiftType.ASR,
            ImmVal(31)
          ), // cmp rdHi, rdLo, asr #31
          BranchAndLink("_errOverflow", Condition.VS) // bne _errOverflow
        )
      }

      case Div(x, y) => {
        Utils.intErrDivZeroFlag = true

        instructions ++= compileExpr(x, operand1Reg)
        instructions += Push(List(operand1Reg))
        instructions ++= compileExpr(y, operand2Reg)
        instructions += Pop(List(operand1Reg))

        instructions ++= List(
          Push(List(R0, R1)),
          Move(R0, operand1Reg),
          Move(R1, operand2Reg),
          Cmp(R1, ImmVal(0)),
          BranchAndLink("_errDivByZero", Condition.EQ),
          BranchAndLink("__aeabi_idiv"),
          Move(resReg, R0),
          Pop(List(R0, R1))
        )
      }

      case Mod(x, y) => {
        Utils.intErrDivZeroFlag = true

        instructions ++= compileExpr(x, operand1Reg)
        instructions += Push(List(operand1Reg))
        instructions ++= compileExpr(y, operand2Reg)
        instructions += Pop(List(operand1Reg))

        instructions ++= List(
          Push(List(R0, R1)),
          Move(R0, operand1Reg),
          Move(R1, operand2Reg),
          Cmp(R1, ImmVal(0)),
          BranchAndLink("_errDivByZero", Condition.EQ),
          BranchAndLink(
            "__aeabi_idivmod"
          ), // signed __aeabi_idivmod(signed numerator, signed denominator)
          Move(resReg, R1),
          Pop(List(R0, R1))
        )
      }

      case Add(x, y) => {
        Utils.intErrOverflowFlag = true

        instructions ++= compileExpr(x, operand1Reg)
        instructions += Push(List(operand1Reg))
        instructions ++= compileExpr(y, operand2Reg)
        instructions += Pop(List(operand1Reg))

        instructions ++= List(
          AddInstr(resReg, operand1Reg, operand2Reg),
          BranchAndLink("_errOverflow", Condition.VS)
        )
      }

      case And(x, y) => {
        instructions ++= compileExpr(x, operand1Reg)
        instructions += Push(List(operand1Reg))
        instructions ++= compileExpr(y, operand2Reg)
        instructions += Pop(List(operand1Reg))

        instructions += AndInstr(resReg, operand1Reg, operand2Reg)
      }

      case Or(x, y) => {
        instructions ++= compileExpr(x, operand1Reg)
        instructions += Push(List(operand1Reg))
        instructions ++= compileExpr(y, operand2Reg)
        instructions += Pop(List(operand1Reg))

        instructions += OrrInstr(resReg, operand1Reg, operand2Reg)
      }

      case Sub(x, y) => {
        Utils.intErrOverflowFlag = true

        instructions ++= compileExpr(x, operand1Reg)
        instructions += Push(List(operand1Reg))
        instructions ++= compileExpr(y, operand2Reg)
        instructions += Pop(List(operand1Reg))

        instructions ++= List(
          SubInstr(resReg, operand1Reg, operand2Reg),
          BranchAndLink("_errOverflow", Condition.VS)
        )
      }

      case LT(x, y) => {
        instructions ++= compileExpr(x, operand1Reg)
        instructions += Push(List(operand1Reg))
        instructions ++= compileExpr(y, operand2Reg)
        instructions += Pop(List(operand1Reg))

        instructions.addAll(
          List(
            Cmp(operand1Reg, operand2Reg),
            Move(resReg, ImmVal(1), Condition.LT),
            Move(resReg, ImmVal(0), Condition.GE)
          )
        )
      }

      case LTE(x, y) => {
        instructions ++= compileExpr(x, operand1Reg)
        instructions += Push(List(operand1Reg))
        instructions ++= compileExpr(y, operand2Reg)
        instructions += Pop(List(operand1Reg))

        instructions.addAll(
          List(
            Cmp(operand1Reg, operand2Reg),
            Move(resReg, ImmVal(1), Condition.LE),
            Move(resReg, ImmVal(0), Condition.GT)
          )
        )
      }

      case GT(x, y) => {
        instructions ++= compileExpr(x, operand1Reg)
        instructions += Push(List(operand1Reg))
        instructions ++= compileExpr(y, operand2Reg)
        instructions += Pop(List(operand1Reg))

        instructions.addAll(
          List(
            Cmp(operand1Reg, operand2Reg),
            Move(resReg, ImmVal(1), Condition.GT),
            Move(resReg, ImmVal(0), Condition.LE)
          )
        )
      }

      case GTE(x, y) =>
        instructions ++= compileExpr(x, operand1Reg)
        instructions += Push(List(operand1Reg))
        instructions ++= compileExpr(y, operand2Reg)
        instructions += Pop(List(operand1Reg))

        instructions.addAll(
          List(
            Cmp(operand1Reg, operand2Reg),
            Move(resReg, ImmVal(1), Condition.GE),
            Move(resReg, ImmVal(0), Condition.LT)
          )
        )

      case NotEqual(x, y) =>
        instructions ++= compileExpr(x, operand1Reg)
        instructions += Push(List(operand1Reg))
        instructions ++= compileExpr(y, operand2Reg)
        instructions += Pop(List(operand1Reg))

        instructions.addAll(
          List(
            Cmp(operand1Reg, operand2Reg),
            Move(resReg, ImmVal(1), Condition.NE),
            Move(resReg, ImmVal(0), Condition.EQ)
          )
        )

      case Equal(x, y) =>
        instructions ++= compileExpr(x, operand1Reg)
        instructions += Push(List(operand1Reg))
        instructions ++= compileExpr(y, operand2Reg)
        instructions += Pop(List(operand1Reg))

        instructions.addAll(
          List(
            Cmp(operand1Reg, operand2Reg),
            Move(resReg, ImmVal(1), Condition.EQ),
            Move(resReg, ImmVal(0), Condition.NE)
          )
        )

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
        instructions += Load(
          resReg,
          OffsetMode(resReg, shiftAmount = ImmVal(-4))
        )
      }
      case Ord(x) => {
        instructions ++= compileExpr(x, resReg)
        // instructions += AndInstr(resReg, operand1Reg, ImmVal(255))
      }

      case Chr(x) => {
        instructions ++= compileExpr(x, resReg)
        // instructions += AndInstr(resReg, operand1Reg, ImmVal(255))
      }

      case IntegerLiter(x) =>
        instructions += Move(resReg, ImmVal(x))

      case BoolLiter(x) => {
        instructions += Move(resReg, ImmVal(if (x) 1 else 0))
      }

      case CharLiter(x) => {
        // instructions += Move(resReg, ImmVal(x.toInt))
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

      case ArrayElem(ident: Ident, xs: List[Expr]) => {
        Utils.arrayFlag = true

        /*


        xs.foreach((dim: Expr) => {
          instructions ++= compileExpr(dim, R0)

          array address -> reg

          instructions += Push(List(R3))

          state.identToReg.get(ident) match {
            case Some(reg) => {
              instructions ++= List(
                Move(R3, reg), //
                BranchAndLink("_arrLoad"),
                Move(resReg, R3),
                Pop(List(R3))
                AddInstr(resReg, resReg, PostIndexedMode(r0, shiftType = Some(LSL), shiftAmount = ImmVal(2)))
              )
            }

            case None => {
            val offset = StackMachine.getIdentOffset(ident) // TODO: PendingStackOffset?
            instructions ++= List(
                  Load(R3, OffsetMode(SP, shiftAmount = ImmVal(offset))),
                  BranchAndLink("_arrLoad"),
                  Move(resReg, R3),
                  Pop(List(R3)
                  AddInstr(R4, R4, PostIndexedMode(r0, shiftType = Some(LSL), shiftAmount = ImmVal(2)))
                  Load(R4, )
                )
              )
            }
          }


        })
         */

        instructions ++= compileExpr(xs.head, R10) // TODO: multidimensional

        instructions += Push(List(R3))

        state.identToReg.get(ident) match {
          case Some(reg) => {
            instructions ++= List(
              Move(R3, reg), //
              BranchAndLink("_arrLoad"),
              Move(resReg, R3),
              Pop(List(R3))
            )
          }
          case None => {
            val offset =
              StackMachine.getIdentOffset(ident) // TODO: PendingStackOffset?
            instructions ++= List(
              Load(R3, OffsetMode(SP, shiftAmount = ImmVal(offset))),
              BranchAndLink("_arrLoad"),
              Move(resReg, R3),
              Pop(List(R3))
            )
          }
        }

        instructions
      }

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
      state: CodeGenState
      // printTable: Map[(Int, Int), Type],
      // symbolTable: Map[Ident, Type],
      // labels: Labels
  ): mutable.ListBuffer[Instruction] = {
    val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer.empty
    state.identToReg.get(ident) match {
      case Some(reg) =>
        if (reg != resReg)
          instructions += Move(resReg, reg)

      case None =>
        instructions += PendingStackOffset(
          Load(
            resReg,
            OffsetMode(
              FP,
              shiftAmount = ImmVal(StackMachine.getIdentOffset(ident))
            )
          )
        )
    }
    instructions
  }

  def storeRes(offset: Int, size: Int, resReg: Register): Instruction = {
    size match {
      case 1 => StoreByte(resReg, OffsetMode(FP, shiftAmount = ImmVal(offset)))
      case _ => Store(resReg, OffsetMode(FP, shiftAmount = ImmVal(offset)))
    }
  }

}
