package wacc.backend

import wacc.AST._
import wacc.backend.Globals.{WORD_SIZE, PAIR_SIZE}
import wacc.backend.Utils
import scala.collection.mutable

object CodeGenerator {
  def compileProgram(
      programNode: Program
  )(implicit state: CodeGenState): mutable.ListBuffer[Instruction] = {
    val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer.empty

    implicit val printTable: Map[(Int, Int), Type] = programNode.printTable
    implicit val symbolTable: Map[Ident, Type] = programNode.symbolTable
    implicit val functionTable: Map[Ident, List[(Type, List[Type])]] =
      programNode.functionTable
    implicit val mainLabels = new Labels("main")

    instructions ++= StackMachine.addStackFrame(programNode.symbolTable)

    instructions ++= compileStats(programNode.stat)

    instructions.mapInPlace(instr =>
      instr match {
        case PendingStackOffset(instr, sf) => {
          instr.putOnStack(sf.currVarOffset)
        }

        case _ => instr
      }
    )

    instructions += Move(R0, ImmVal(0))

    instructions ++= StackMachine.removeStackFrame()

    instructions += Pop(List(PC))

    val funcInstructions = mutable.ListBuffer.empty[Instruction]

    programNode.funcs.foreach(func => {
      funcInstructions ++= compileFunc(func)(
        new CodeGenState,
        programNode.functionTable
      )
    })

    instructions ++= funcInstructions
    Utils.addUtils()(instructions)

    mainLabels.addLabelInstructions(instructions)
    instructions
  }

  private def funcLabel(funcIdent: Ident, argTypes: List[Type]): String = {
    val argTypesAsStr = argTypes match {
      case Nil => List("void") // should never happen
      case _ => {
        argTypes.map(argType =>
          argType match {
            case arr: ArrayType =>
              arr.labelToString() // array type uses different label string method
            case _ => argType.toString
          }
        )
      }
    }
    "wacc_" + funcIdent.name + "_" + argTypesAsStr.mkString("_")
  }

  def compileFunc(funcNode: Func)(implicit
      state: CodeGenState,
      functionTable: Map[Ident, List[(Type, List[Type])]]
  ): mutable.ListBuffer[Instruction] = {
    val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer.empty
    val funcNameLabel = funcLabel(funcNode.ident, funcNode.paramList.map(_.ty))
    val paramsAndRegs = funcNode.paramList zip List(R0, R1, R2, R3)

    paramsAndRegs.foreach { case ((Param(_, ident), reg)) =>
      state.identToReg += (ident -> reg)
    }

    instructions ++= List(
      Label(funcNameLabel),
      Push(List(FP, LR)),
      Push(List(R4, R5, R6, R7)),
      Move(FP, SP)
    )

    state.funcLabel = funcNameLabel
    StackMachine.addStackFrame(funcNode.symbolTable, funcNode.paramList, true)
    val funcLabels = new Labels(funcNameLabel)
    instructions ++= compileStats(funcNode.stats)(
      state,
      funcNode.printTable,
      funcNode.symbolTable,
      functionTable,
      funcLabels
    )
    funcLabels.addLabelInstructions(instructions)
    StackMachine.removeStackFrame(true)

    instructions ++= List(
      Label(state.funcLabel + "_exit"),
      Move(SP, FP),
      Pop(List(R4, R5, R6, R7)),
      Pop(List(FP, PC)),
      Directive("ltorg")
    )

    instructions
  }

  def compileStats(stats: List[Stat], func: Boolean = false)(implicit
      state: CodeGenState,
      printTable: Map[(Int, Int), Type],
      symbolTable: Map[Ident, Type],
      functionTable: Map[Ident, List[(Type, List[Type])]],
      labels: Labels
  ): mutable.ListBuffer[Instruction] = {
    val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer.empty

    stats.foreach(instructions ++= compileStat(_))

    instructions.mapInPlace(instr =>
      instr match {
        case PendingStackOffset(instr, sf) =>
          instr.putOnStack(sf.currVarOffset)
        case _ => instr
      }
    )

    instructions
  }

  def compileStat(stat: Stat)(implicit
      state: CodeGenState,
      printTable: Map[(Int, Int), Type],
      symbolTable: Map[Ident, Type],
      functionTable: Map[Ident, List[(Type, List[Type])]],
      labels: Labels
  ): mutable.ListBuffer[Instruction] = {
    val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer.empty
    stat match {
      case Skip() => instructions

      case Declare(ty, ident, rValue) => {
        if (state.identToReg.contains(ident)) {
          state.identToReg -= ident
        } else if (
          StackMachine.stackFrameList.last.declaredVarMap.contains(ident)
        ) {
          StackMachine.stackFrameList.last.declaredVarMap -= ident
        }

        state.getRegOrNone match {
          case Some(scratchReg) => {
            instructions ++= compileRValue(rValue, scratchReg)
            state.identToReg += (ident -> scratchReg)
          }
          case None => {
            val offset = StackMachine.addDeclaration(ident, ty.size)
            instructions ++= compileRValue(rValue, state.tmp)
            instructions += PendingStackOffset(
              storeRes(offset, ty.size, state.tmp),
              StackMachine.stackFrameList.last
            )
          }
        }
      }

      case Assign(lValue, rValue) => {
        instructions ++= compileRValue(rValue, state.tmp)

        var assignByte = getLValueSize(lValue) == 1
        assignByte = assignByte || (rValue match {
          case ident: Ident => {
            symbolTable.get(ident) match {
              case Some(BoolType()) => true
              case Some(CharType()) => true
              case _                => false
            }
          }
          case _: BoolLiter => true
          case _: CharLiter => true
          case _            => false
        })

        lValue match {
          case ident: Ident => {
            state.identToReg.get(ident) match {
              case Some(reg) => {
                instructions += Move(reg, state.tmp)
              }
              case None => {
                val offset = StackMachine.getIdentOffset(ident)
                instructions += PendingStackOffset(
                  storeRes(offset, ident.size, state.tmp),
                  StackMachine.stackFrameList.last
                )
              }
            }
          }
          case _: PairElem => {
            instructions ++= compileLValue(lValue, state.tmp2, assignByte)
            instructions += (if (assignByte)
                               StoreByte(state.tmp, OffsetMode(state.tmp2))
                             else Store(state.tmp, OffsetMode(state.tmp2)))
          }
          case _ => {
            instructions ++= compileLValue(lValue, state.tmp2, assignByte)
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

          case _ => throw new Exception("Read: invalid type")
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
                  storeRes(offset, ident.size, R0),
                  StackMachine.stackFrameList.last
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

      case Free(expr) => {
        Utils.freePairFlag = true
        Utils.errNullFlag = true
        Utils.printStringFlag = true

        instructions += Push(List(R0))
        expr match {
          case ident: Ident => {
            symbolTable.get(ident) match {
              case Some(PairType(_, _)) => {
                instructions ++= compileExpr(expr, state.tmp)
                instructions += Move(R0, state.tmp)
                instructions += BranchAndLink("_freepair")
              }
              case Some(ArrayType(_)) => {
                instructions ++= compileExpr(expr, state.tmp)
                instructions += SubInstr(R0, state.tmp, ImmVal(4))
                instructions += BranchAndLink("free")
              }
              case _ => throw new Exception("Freeing non-pair or non-array")
            }
          }
          case _ => throw new Exception("Freeing non-pair or non-array")
        }
        instructions += Pop(List(R0))

      }

      case Return(expr) => {
        instructions ++= compileExpr(expr, state.tmp)
        instructions += Move(R0, state.tmp)
        instructions += BranchAndLink(state.funcLabel + "_exit")
      }
      case Exit(expr) => {
        instructions ++= compileExpr(expr, state.tmp)
        instructions += Move(R0, state.tmp)
        instructions += BranchAndLink("exit")
      }

      case Print(expr) => instructions ++= genPrint(expr)

      case Println(expr) => instructions ++= genPrint(expr, true)

      case ifStatNode @ If(_, _, _) =>
        instructions ++= compileIfStat(
          ifStatNode,
          state.tmp3
        )

      case whileNode @ While(cond, bodyStat) => {
        val uniqueWhileName = state.funcLabel + "while_" + state.getNewLabelId;
        val condLabel = uniqueWhileName + "_cond"
        val bodyLabel = uniqueWhileName + "_body"

        val oldAvailableRegs = state.availableRegs
        val oldIdentToReg: mutable.Map[Ident, Register] = mutable.Map.empty
        oldIdentToReg ++= state.identToReg
        val oldDeclaredVars: Map[Ident, Int] =
          StackMachine.stackFrameList.last.declaredVarMap

        instructions += Branch(condLabel)

        instructions += Label(bodyLabel)

        bodyStat.foreach(stat =>
          instructions ++= compileStat(stat)(
            state,
            printTable,
            whileNode.symbolTable,
            functionTable,
            labels
          )
        )

        val condReg = state.tmp3

        instructions += Label(condLabel)
        instructions ++= compileExpr(cond, condReg)

        instructions.addAll(
          List(
            Cmp(condReg, ImmVal(1)),
            Branch(bodyLabel, Condition.EQ)
          )
        )

        state.availableRegs = oldAvailableRegs
        state.identToReg.clear()
        oldIdentToReg.foreach { case (id, reg) =>
          state.identToReg += (id -> reg)
        }
        StackMachine.stackFrameList.last.declaredVarMap = oldDeclaredVars
      }

      case scopeNode @ Scope(stats) => {

        val oldAvailableRegs = state.availableRegs
        val oldIdentToReg: mutable.Map[Ident, Register] = mutable.Map.empty
        oldIdentToReg ++= state.identToReg
        val oldDeclaredVars: Map[Ident, Int] =
          StackMachine.stackFrameList.last.declaredVarMap

        instructions ++= StackMachine.addStackFrame(scopeNode.symbolTable)
        instructions ++= compileStats(stats)(
          state,
          printTable,
          scopeNode.symbolTable,
          functionTable,
          labels
        )
        instructions ++= StackMachine.removeStackFrame()

        state.availableRegs = oldAvailableRegs
        state.identToReg.clear()
        oldIdentToReg.foreach { case (id, reg) =>
          state.identToReg += (id -> reg)
        }
        StackMachine.stackFrameList.last.declaredVarMap = oldDeclaredVars
      }
    }
    instructions
  }

  def compileRValue(rValue: RValue, resReg: Register)(implicit
      state: CodeGenState,
      printTable: Map[(Int, Int), Type],
      symbolTable: Map[Ident, Type],
      functionTable: Map[Ident, List[(Type, List[Type])]],
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
              Push(List(R0, R1, R2, R3)),
              Move(R0, ImmVal(WORD_SIZE + arraySize)),
              BranchAndLink("malloc"),
              Move(arrayStartReg, R0),
              Pop(List(R0, R1, R2, R3)),
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
        instructions ++= compileNewPair(expr1, expr2, resReg)
      case call: Call =>
        instructions ++= compileFunctionCall(call, resReg)
      case expr: Expr => instructions ++= compileExpr(expr, resReg)
      case pair: PairElem =>
        instructions ++= getPairElem(pair, resReg, unpack = true)
    }

    instructions
  }

  private def getPairElem(
      pairElem: PairElem,
      resReg: Register,
      unpack: Boolean = false
  )(implicit
      state: CodeGenState,
      printTable: Map[(Int, Int), Type],
      symbolTable: Map[Ident, Type],
      functionTable: Map[Ident, List[(Type, List[Type])]],
      labels: Labels
  ): mutable.ListBuffer[Instruction] = {
    val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer.empty
    Utils.errNullFlag = true

    pairElem match {
      case Fst(lv) => {
        lv match {
          case innerPair: PairElem =>
            instructions ++= getPairElem(innerPair, state.tmp, unpack = true)
          case arrayElem: ArrayElem => {
            instructions ++= compileLValue(arrayElem, state.tmp, load = true)
          }
          case _ => {
            instructions ++= compileLValue(lv, state.tmp)
          }
        }
        instructions += Cmp(state.tmp, ImmVal(0))
        instructions += BranchAndLink("_errNull", Condition.EQ)
        instructions += Load(state.tmp2, OffsetMode(state.tmp))

        if (unpack) {
          instructions += (getLValueSize(lv) match {
            case 1 => LoadByte(resReg, OffsetMode(state.tmp2))
            case _ => Load(resReg, OffsetMode(state.tmp2))
          })
        }
      }

      case Snd(lv) => {
        lv match {
          case innerPair: PairElem =>
            instructions ++= getPairElem(innerPair, state.tmp, unpack)
          case arrayElem: ArrayElem => {
            instructions ++= compileLValue(arrayElem, state.tmp, load = true)
          }
          case _ => {
            instructions ++= compileLValue(lv, state.tmp)
          }
        }
        instructions += Cmp(state.tmp, ImmVal(0))
        instructions += BranchAndLink("_errNull", Condition.EQ)
        instructions += Load(
          state.tmp2,
          OffsetMode(state.tmp, shiftAmount = ImmVal(WORD_SIZE))
        )

        if (unpack) {
          instructions += (getLValueSize(lv) match {
            case 1 => LoadByte(resReg, OffsetMode(state.tmp2))
            case _ => Load(resReg, OffsetMode(state.tmp2))
          })
        }
      }
    }

    instructions
  }

  def compileLValue(
      lValue: LValue,
      resReg: Register,
      assignByte: Boolean = false,
      load: Boolean = false
  )(implicit
      state: CodeGenState,
      printTable: Map[(Int, Int), Type],
      symbolTable: Map[Ident, Type],
      functionTable: Map[Ident, List[(Type, List[Type])]],
      labels: Labels
  ): mutable.ListBuffer[Instruction] = {
    val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer.empty

    lValue match {
      case ident: Ident => instructions ++= compileIdent(ident, resReg)
      case ArrayElem(ident: Ident, xs: List[Expr]) =>
        Utils.arrayFlag = true

        instructions ++= compileExpr(xs.head, R10)

        if (resReg != R3)
          instructions += Push(List(R3))

        state.identToReg.get(ident) match {
          case Some(reg) => if (reg != R3) instructions += Move(R3, reg)
          case None =>
            instructions += Move(
              R3,
              ImmVal(StackMachine.getIdentOffset(ident))
            )
        }

        if (load) {
          print(s"load: $lValue ${getLValueSize(lValue)}")
          getLValueSize(lValue) match {
            case 1 => instructions += BranchAndLink("_arrLoadB")
            case _ => instructions += BranchAndLink("_arrLoad")
          }
        } else {
          if (assignByte) {
            instructions += BranchAndLink("_arrStoreB")
          } else {
            instructions += BranchAndLink("_arrStore")
          }
        }

        if (resReg != R3)
          instructions ++= List(
            Move(resReg, R3),
            Pop(List(R3))
          )

      case pairElem: PairElem => {
        instructions += Push(List(state.tmp))
        instructions ++= getPairElem(pairElem, state.tmp2)
        instructions += Pop(List(state.tmp))
      }
    }
    instructions
  }

  def compileNewPair(expr1: Expr, expr2: Expr, resReg: Register)(implicit
      state: CodeGenState,
      printTable: Map[(Int, Int), Type],
      symbolTable: Map[Ident, Type],
      functionTable: Map[Ident, List[(Type, List[Type])]],
      labels: Labels
  ): mutable.ListBuffer[Instruction] = {
    val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer.empty

    def compileExprForPair(expr: Expr): mutable.ListBuffer[Instruction] = {
      val instructions: mutable.ListBuffer[Instruction] =
        mutable.ListBuffer.empty
      instructions ++= List(
        Push(List(R0, R1, R2, R3)),
        Move(R0, ImmVal(expr.size)),
        BranchAndLink("malloc"),
        Move(state.tmp3, R0),
        Pop(List(R0, R1, R2, R3))
      )

      instructions ++= compileExpr(expr, state.tmp)

      instructions ++= List(
        expr.size match {
          case Globals.WORD_SIZE => Store(state.tmp, OffsetMode(state.tmp3))
          case Globals.CHAR_SIZE => StoreByte(state.tmp, OffsetMode(state.tmp3))
        },
        Move(state.tmp, state.tmp3),
        Push(List(state.tmp))
      )
      instructions
    }

    instructions ++= compileExprForPair(expr1)
    instructions ++= compileExprForPair(expr2)

    instructions ++= List(
      Push(List(R0, R1, R2, R3)),
      Move(R0, ImmVal(PAIR_SIZE)),
      BranchAndLink("malloc"),
      Move(state.tmp3, R0),
      Pop(List(R0, R1, R2, R3)),
      Pop(List(state.tmp)),
      Store(state.tmp, OffsetMode(state.tmp3, shiftAmount = ImmVal(WORD_SIZE))),
      Pop(List(state.tmp)),
      Store(state.tmp, OffsetMode(state.tmp3)),
      Move(resReg, state.tmp3)
    )
  }

  def compileFunctionCall(funcCallNode: Call, resReg: Register)(implicit
      state: CodeGenState,
      printTable: Map[(Int, Int), Type],
      symbolTable: Map[Ident, Type],
      functionTable: Map[Ident, List[(Type, List[Type])]],
      labels: Labels
  ): mutable.ListBuffer[Instruction] = {
    val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer.empty
    val numArgs = funcCallNode.args.length
    var argShift = 0

    // push the reversed args onto stack
    // add stack pointer back to original pos (handled by stack machine)
    // move resreg r0 (handled by assign)

    instructions += Push(List(R0, R1, R2, R3))

    instructions ++= StackMachine.addStackFrame(symbolTable)

    val regs = mutable.Stack(R0, R1, R2, R3)
    val regsLength = regs.length

    for (argument <- funcCallNode.args.take(regsLength).reverse) {
      instructions ++= compileExpr(argument, state.tmp)
      instructions += Push(List(state.tmp))
    }

    for (argument <- funcCallNode.args.take(regsLength).reverse) {
      instructions += Pop(List(regs.pop()))
    }

    if (regs.isEmpty) {
      for (argument <- funcCallNode.args.drop(regsLength).reverse) {
        val argSize = argument.size
        argShift += argSize
        instructions ++= compileExpr(argument, state.tmp) // mov r8, arg
        if (argSize == 1) {
          instructions += StoreByte(
            state.tmp,
            PostIndexedMode(SP, shiftAmount = ImmVal(-argSize))
          ) // strb r8 [sp, -1]!
        } else {
          instructions += Store(
            state.tmp,
            PostIndexedMode(SP, shiftAmount = ImmVal(-argSize))
          ) // str r8 [sp, -4]!
        }
      }
    }

    val argTypes: List[Type] = funcCallNode.argTypes
    val funcLabelName = funcLabel(funcCallNode.x, argTypes)

    instructions += BranchAndLink(funcLabelName)
    instructions += Move(resReg, R0)

    if (numArgs > regsLength) {
      instructions += AddInstr(SP, SP, ImmVal(argShift))
    }

    instructions ++= StackMachine.removeStackFrame(true)
    instructions += Pop(List(R0, R1, R2, R3))

  }

  def compileExpr(expr: Expr, resReg: Register)(implicit
      state: CodeGenState,
      printTable: Map[(Int, Int), Type],
      symbolTable: Map[Ident, Type],
      functionTable: Map[Ident, List[(Type, List[Type])]],
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
          BranchAndLink("_errOverflow", Condition.NE) // blne _errOverflow
        )
      }

      case Div(x, y) => {
        Utils.intErrDivZeroFlag = true

        instructions ++= compileExpr(x, operand1Reg)
        instructions += Push(List(operand1Reg))
        instructions ++= compileExpr(y, operand2Reg)
        instructions += Pop(List(operand1Reg))

        instructions ++= List(
          Push(List(R0, R1, R2, R3)),
          Move(R0, operand1Reg),
          Move(R1, operand2Reg),
          Cmp(R1, ImmVal(0)),
          BranchAndLink("_errDivByZero", Condition.EQ),
          BranchAndLink("__aeabi_idiv"),
          Move(resReg, R0),
          Pop(List(R0, R1, R2, R3))
        )
      }

      case Mod(x, y) => {
        Utils.intErrDivZeroFlag = true

        instructions ++= compileExpr(x, operand1Reg)
        instructions += Push(List(operand1Reg))
        instructions ++= compileExpr(y, operand2Reg)
        instructions += Pop(List(operand1Reg))

        instructions ++= List(
          Push(List(R0, R1, R2, R3)),
          Move(R0, operand1Reg),
          Move(R1, operand2Reg),
          Cmp(R1, ImmVal(0)),
          BranchAndLink("_errDivByZero", Condition.EQ),
          BranchAndLink(
            "__aeabi_idivmod"
          ), // signed __aeabi_idivmod(signed numerator, signed denominator)
          Move(resReg, R1),
          Pop(List(R0, R1, R2, R3))
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
        Utils.intErrOverflowFlag = true
        instructions ++= compileExpr(x, resReg)
        instructions += Rsb(resReg, resReg, ImmVal(0))
        instructions += BranchAndLink("_errOverflow", Condition.VS)
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
      }

      case Chr(x) => {
        instructions ++= compileExpr(x, resReg)
      }

      case IntegerLiter(x) =>
        if (x.abs > 255)
          instructions += Load(resReg, LoadImmVal(x)) // for overflow
        else
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
        instructions += Move(resReg, ImmVal(0))

      case a @ ArrayElem(ident: Ident, xs: List[Expr]) => {
        Utils.arrayFlag = true

        xs match {
          case head :: tail => {
            instructions ++= compileExpr(head, R10)

            if (resReg != R3)
              instructions += Push(List(R3))

            state.identToReg.get(ident) match {
              case Some(reg) => if (reg != R3) instructions += Move(R3, reg)
              case None =>
                symbolTable.get(ident).get.size match {
                  case 1 =>
                    instructions += PendingStackOffset(
                      LoadByte(
                        R3,
                        OffsetMode(
                          FP,
                          shiftAmount =
                            ImmVal(StackMachine.getIdentOffset(ident))
                        )
                      ),
                      StackMachine.stackFrameList.last
                    )
                  case _ =>
                    instructions += PendingStackOffset(
                      Load(
                        R3,
                        OffsetMode(
                          FP,
                          shiftAmount =
                            ImmVal(StackMachine.getIdentOffset(ident))
                        )
                      ),
                      StackMachine.stackFrameList.last
                    )
                }

            }

            instructions ++= List(
              a.actualSize match {
                case 1 => BranchAndLink("_arrLoadB")
                case _ => BranchAndLink("_arrLoad")
              },
              (if (tail == Nil) Move(resReg, R3) else Move(state.tmp, R3))
            )

            if (resReg != R3)
              instructions += Pop(List(R3))

            tail.zipWithIndex.foreach({
              case (dimIndex: Expr, i: Int) => {
                if (resReg != R3)
                  instructions += Push(List(R3))

                instructions += Push(List(state.tmp))
                instructions ++= compileExpr(dimIndex, R10)
                instructions += Pop(List(state.tmp))

                instructions ++= List(
                  Move(R3, state.tmp),
                  a.actualSize match {
                    case 1 => BranchAndLink("_arrLoadB")
                    case _ => BranchAndLink("_arrLoad")
                  },
                  if (i == tail.length - 1) Move(resReg, R3) else Move(R10, R3)
                )

                if (resReg != R3)
                  instructions += Pop(List(R3))
              }
            })
          }
          case Nil =>
            throw new Exception("ArrayElem with no dimensions")
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
      functionTable: Map[Ident, List[(Type, List[Type])]],
      labels: Labels
  ): mutable.ListBuffer[Instruction] = {
    val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer.empty

    val oldAvailableRegs = state.availableRegs
    val oldIdentToReg: mutable.Map[Ident, Register] = mutable.Map.empty
    oldIdentToReg ++= state.identToReg
    val oldDeclaredVars: Map[Ident, Int] =
      StackMachine.stackFrameList.last.declaredVarMap

    oldIdentToReg ++= state.identToReg

    val condReg = state.tmp

    // Compile condition
    instructions ++= compileExpr(ifNode.cond, condReg)

    val thenLabel = "l_" + state.funcLabel + "_" + state.getNewLabelId
    val endLabel = "l_" + state.funcLabel + "_" + state.getNewLabelId

    instructions.addAll(
      List(
        Cmp(condReg, ImmVal(1)),
        Branch(thenLabel, Condition.EQ)
      )
    )

    // Compile else statement
    ifNode.elseStat.foreach(stat =>
      instructions ++= compileStat(stat)(
        state,
        printTable,
        ifNode.elseSymbolTable,
        functionTable,
        labels
      )
    )

    instructions.addAll(
      List(
        Branch(endLabel),
        Label(thenLabel)
      )
    )

    // Compile then statement
    ifNode.thenStat.foreach(stat =>
      instructions ++= compileStat(stat)(
        state,
        printTable,
        ifNode.thenSymbolTable,
        functionTable,
        labels
      )
    )

    instructions += Label(endLabel)

    state.availableRegs = oldAvailableRegs
    state.identToReg.clear()
    oldIdentToReg.foreach { case (id, reg) =>
      state.identToReg += (id -> reg)
    }
    StackMachine.stackFrameList.last.declaredVarMap = oldDeclaredVars

    instructions

  }

  def compileIdent(ident: Ident, resReg: Register)(implicit
      state: CodeGenState,
      symbolTable: Map[Ident, Type]
  ): mutable.ListBuffer[Instruction] = {
    val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer.empty
    state.identToReg.get(ident) match {
      case Some(reg) =>
        if (reg != resReg)
          instructions += Move(resReg, reg)

      case None =>
        symbolTable.get(ident).get.size match {
          case 1 => {
            instructions += PendingStackOffset(
              LoadByte(
                resReg,
                OffsetMode(
                  FP,
                  shiftAmount = ImmVal(StackMachine.getIdentOffset(ident))
                )
              ),
              StackMachine.stackFrameList.last
            )
          }
          case _ => {
            instructions += PendingStackOffset(
              Load(
                resReg,
                OffsetMode(
                  FP,
                  shiftAmount = ImmVal(StackMachine.getIdentOffset(ident))
                )
              ),
              StackMachine.stackFrameList.last
            )
          }

        }
    }
    instructions
  }

  private def genPrint(expr: Expr, println: Boolean = false)(implicit
      state: CodeGenState,
      printTable: Map[(Int, Int), Type],
      symbolTable: Map[Ident, Type],
      functionTable: Map[Ident, List[(Type, List[Type])]],
      labels: Labels
  ): mutable.ListBuffer[Instruction] = {
    val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer.empty

    val exprType: Option[Type] = expr match {
      case ident: Ident => {
        symbolTable.get(ident) match {
          case Some(ty) => Some(ty)
          case None     => printTable.get(expr.pos)
        }
      }
      case _ => {
        printTable.get(expr.pos)
      }
    }

    instructions ++= compileExpr(expr, state.tmp)
    instructions += Push(List(R0, R1, R2, R3))
    instructions += Move(R0, state.tmp)
    exprType match {
      case Some(IntType()) =>
        if (!Utils.printIntFlag)
          Utils.printIntFlag = true
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
      case _ =>
        if (!Utils.printPFlag)
          Utils.printPFlag = true
        instructions += BranchAndLink("_printp")
    }
    if (println) {
      if (!Utils.printlnFlag)
        Utils.printlnFlag = true
      instructions += BranchAndLink("_println")
    }
    instructions += Pop(List(R0, R1, R2, R3))
  }

  private def storeRes(
      offset: Int,
      size: Int,
      resReg: Register
  ): Instruction = {
    size match {
      case 1 => StoreByte(resReg, OffsetMode(FP, shiftAmount = ImmVal(offset)))
      case _ => Store(resReg, OffsetMode(FP, shiftAmount = ImmVal(offset)))
    }
  }

  private def getLValueSize(
      lValue: LValue
  )(implicit symbolTable: Map[Ident, Type]): Int = {
    lValue match {
      case id: Ident => {
        symbolTable.get(id) match {
          case Some(t) => t.size
          case None => {
            StackMachine.getIdentOffset(id)
          }
        }
      }

      case array: ArrayElem => array.actualSize

      case pElem: PairElem =>
        pElem match {
          case Fst(x) => getLValueSize(x)
          case Snd(y) => getLValueSize(y)
        }
    }
  }

}
