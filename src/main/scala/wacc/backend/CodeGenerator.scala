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
        instructions += Comment("identToRegs: " + state.identToReg)
      }

      case Assign(lValue, rValue) => {
        instructions += Comment("Assign: compiling RVALUE " + rValue)
        instructions ++= compileRValue(rValue, state.tmp)
        
        println("lValue: " + lValue + " size: " + getLValueSize(lValue))
        var assignByte = getLValueSize(lValue) == 1
        assignByte = assignByte || (rValue match {
          case ident: Ident => {
            symbolTable.get(ident) match {
              case Some(BoolType()) => true
              case Some(CharType()) => true
              case _ => false
            }
          }
          case _: BoolLiter => true
          case _: CharLiter => true
          case _ => false
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
                  storeRes(offset, ident.size, state.tmp)
                )
              }
            }
          }
          case _: PairElem => {
            instructions += Comment("Assign: compiling LVALUE " + lValue)
            instructions ++= compileLValue(lValue, state.tmp2, assignByte)
            instructions += (if (assignByte) StoreByte(state.tmp, OffsetMode(state.tmp2)) else Store(state.tmp, OffsetMode(state.tmp2)))
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

      case Free(expr) => {
        Utils.freePairFlag = true
        Utils.errNullFlag = true
        Utils.printStringFlag = true
        instructions ++= compileExpr(expr, state.tmp)
        instructions += Move(R0, state.tmp)
        instructions += BranchAndLink("_freepair")
      }

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
        instructions ++= compileExpr(expr, state.tmp)
        instructions += Push(List(R0, R1))
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

          case _ =>
            if (!Utils.printPFlag) {
              Utils.printPFlag = true
              labels.addDataMsg("%p\u0000")
            }
            instructions += BranchAndLink("_printp")
        }
        instructions += Pop(List(R0, R1))
      }

      case Println(expr) => {
        instructions ++= compileExpr(expr, state.tmp)
        instructions += Push(List(R0, R1))
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
        instructions += Pop(List(R0, R1))
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

        bodyStat.foreach(stat => instructions ++= compileStat(stat)(state, printTable, whileNode.symbolTable, labels))

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
              Push(List(R0, R1)),
              Move(R0, ImmVal(WORD_SIZE + arraySize)),
              BranchAndLink("malloc"),
              Move(arrayStartReg, R0),
              Pop(List(R0, R1)),
              AddInstr(arrayStartReg, arrayStartReg, ImmVal(WORD_SIZE)),
              Move(a_reg, ImmVal(xs.length)),
              Store(
                a_reg,
                OffsetMode(arrayStartReg, shiftAmount = ImmVal(-4))
                ),
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
      case Call(_, _) =>
        instructions ++= compileFunctionCall(rValue.asInstanceOf[Call])
      case expr: Expr     => instructions ++= compileExpr(expr, resReg)
      case pair: PairElem => {
        // instructions += Push(List(R8))
        instructions ++= getPairElem(pair, resReg, unpack = true)
        // instructions += Pop(List(R8))
      }
    }

    instructions
  }

  private def getPairElem(pairElem: PairElem, resReg: Register, unpack: Boolean = false)(implicit
      state: CodeGenState,
      printTable: Map[(Int, Int), Type],
      symbolTable: Map[Ident, Type],
      labels: Labels
  ): mutable.ListBuffer[Instruction] = {
    val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer.empty
    Utils.errNullFlag = true

    pairElem match {
      case Fst(lv) => {
        lv match {
          case innerPair: PairElem => instructions ++= getPairElem(innerPair, state.tmp, unpack = true)
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
          case innerPair: PairElem => instructions ++= getPairElem(innerPair, state.tmp, unpack)
          case arrayElem: ArrayElem => {
            instructions ++= compileLValue(arrayElem, state.tmp, load = true)
          }
          case _ => {
            instructions ++= compileLValue(lv, state.tmp)
          }
        }
        instructions += Cmp(state.tmp, ImmVal(0))
        instructions += BranchAndLink("_errNull", Condition.EQ)
        instructions += Load(state.tmp2, OffsetMode(state.tmp, shiftAmount = ImmVal(WORD_SIZE)))

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
  
  def compileLValue(lValue: LValue, resReg: Register, assignByte: Boolean = false, load: Boolean = false)(implicit
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

        instructions ++= compileExpr(xs.head, R10) // TODO: multidimensional

        instructions += Push(List(R3))

        state.identToReg.get(ident) match {
          case Some(reg) => instructions += Move(R3, reg)
          case None =>
            instructions += Move(
              R3,
              ImmVal(StackMachine.getIdentOffset(ident))
            )
        }

        if (load) {
          instructions += BranchAndLink("_arrLoad")
        } else {
          if (assignByte) {
            instructions += BranchAndLink("_arrStoreB") 
          } else { 
            instructions += BranchAndLink("_arrStore")
          }
        }

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
      labels: Labels
  ): mutable.ListBuffer[Instruction] = {
      val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer.empty
      
      def compileExprForPair(expr: Expr): mutable.ListBuffer[Instruction] = {
        val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer.empty
        instructions ++= List(
          Push(List(R0, R1)),
          Move(R0, ImmVal(expr.size)),
          BranchAndLink("malloc"),
          Move(state.tmp3, R0),
          Pop(List(R0, R1)),
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
        Push(List(R0, R1)),
        Move(R0, ImmVal(PAIR_SIZE)),
        BranchAndLink("malloc"),
        Move(state.tmp3, R0),
        Pop(List(R0, R1)),
        Pop(List(state.tmp)),
        Store(state.tmp, OffsetMode(state.tmp3, shiftAmount = ImmVal(WORD_SIZE))),
        Pop(List(state.tmp)),
        Store(state.tmp, OffsetMode(state.tmp3)),
        Move(resReg, state.tmp3),
      )
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
        // instructions += AndInstr(resReg, operand1Reg, ImmVal(255))
      }

      case Chr(x) => {
        instructions ++= compileExpr(x, resReg)
        // instructions += AndInstr(resReg, operand1Reg, ImmVal(255))
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
        instructions += Move(resReg, ImmVal(0))

      case ArrayElem(ident: Ident, xs: List[Expr]) => {
        Utils.arrayFlag = true

        xs match {
          case head :: tail => {
            instructions ++= compileExpr(head, R10)

            instructions += Push(List(R3))

            state.identToReg.get(ident) match {
              case Some(reg) => instructions += Move(R3, reg)
              case None   => instructions += Load(R3, OffsetMode(SP, shiftAmount = ImmVal(StackMachine.getIdentOffset(ident))))
            }

            instructions ++= List(
              BranchAndLink("_arrLoad"),
              (if (tail == Nil) Move(resReg, R3) else Move(state.tmp, R3)),
              Pop(List(R3))
            )

            tail.zipWithIndex.foreach({
              case (dimIndex: Expr, i: Int) => {
                instructions += Push(List(R3))

                // should encapsulate with push and pop r8s
                instructions += Push(List(state.tmp))
                instructions ++= compileExpr(dimIndex, R10)
                instructions += Pop(List(state.tmp))

                instructions ++= List(
                  Move(R3, state.tmp),
                  BranchAndLink("_arrLoad"),
                  if (i == tail.length - 1) Move(resReg, R3) else Move(R10, R3),
                  Pop(List(R3))
                )
              }
            })
          }
          case Nil => {
            // should not happen
            throw new Exception("ArrayElem with no dimensions")
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

      
  def getLValueSize(lValue: LValue)(implicit symbolTable: Map[Ident, Type]): Int ={
    lValue match {
      case id: Ident => {
        symbolTable.get(id).get.size
      }
        
      case array: ArrayElem => array.actualSize
      
      case pElem: PairElem => pElem match {
        case Fst(x) => getLValueSize(x)
        case Snd(y) => getLValueSize(y)
      }
    }
  }

}
