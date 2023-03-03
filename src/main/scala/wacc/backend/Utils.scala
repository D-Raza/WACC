package wacc.backend
import wacc.backend._
import wacc.backend.Globals.{WORD_SIZE, CHAR_SIZE}
import wacc.backend.Condition.NE

import scala.collection.mutable

object Utils {

  var printStringFlag = false
  var printIntFlag = false
  var printCharFlag = false
  var printBoolFlag = false
  var printlnFlag = false
  var printRefFlag = false
  var printPFlag = false
  var readIntFlag = false
  var readCharFlag = false
  var intErrOverflowFlag = false
  var intErrDivZeroFlag = false
  var arrayFlag = false
  var freePairFlag = false
  var errNullFlag = false

  def addUtils()(implicit
      instructions: mutable.ListBuffer[Instruction]
  ): Unit = {
    if (freePairFlag) {
      errNullFlag = true
      freePair()
    }
    if (errNullFlag) {
      printStringFlag = true
      errNull()
    }
    if (
      printStringFlag
      || intErrOverflowFlag
      || intErrDivZeroFlag
    ) {
      printString()
    }
    if (printIntFlag) {
      printInt()
    }
    if (printCharFlag) {
      printChar()
    }
    if (printBoolFlag) {
      printBool()
    }
    if (printlnFlag) {
      printLine()
    }
    if (printPFlag) {
      printP()
    }
    if (readIntFlag) {
      readInt()
    }
    if (readCharFlag) {
      readChar()
    }
    if (arrayFlag) {
      arrStore()
      arrStoreB()
      arrLoad()
      boundsCheck()
    }
    if (intErrOverflowFlag) {
      intErrOverflow()
    }
    if (intErrDivZeroFlag) {
      intErrDivZero()
    }

  }

  private def printString()(implicit
      instructions: mutable.ListBuffer[Instruction]
  ): Unit = {
    instructions.addAll(
      List(
        Directive("data"),
        Directive("    word 4"),
        Label(".L._prints_str0"),
        Directive(s"    asciz \"%.*s\""),
        Directive("text"),
        Label("_prints"),
        Push(List(LR)),
        Move(R2, R0),
        Load(R1, PreIndexedMode(R0, shiftAmount = ImmVal(-WORD_SIZE))),
        Load(R0, LabelOp(".L._prints_str0")),
        BranchAndLink("printf"),
        Move(R0, ImmVal(0)),
        BranchAndLink("fflush"),
        Pop(List(PC))
      )
    )
  }

  private def printInt()(implicit
      instructions: mutable.ListBuffer[Instruction]
  ): Unit = {
    instructions.addAll(
      List(
        Directive("data"),
        Directive("    word 2"),
        Label(".L._printi_str0"),
        Directive(s"    asciz \"%d\""),
        Directive("text"),
        Label("_printi"),
        Push(List(LR)),
        Move(R1, R0),
        Load(R0, LabelOp(".L._printi_str0")),
        BranchAndLink("printf"),
        Move(R0, ImmVal(0)),
        BranchAndLink("fflush"),
        Pop(List(PC))
      )
    )
  }

  private def printChar()(implicit
      instructions: mutable.ListBuffer[Instruction]
  ): Unit = {
    instructions.addAll(
      List(
        Directive("data"),
        Directive("    word 2"),
        Label(".L._printc_str0"),
        Directive(s"    asciz \"%c\""),
        Directive("text"),
        Label("_printc"),
        Push(List(LR)),
        Move(R1, R0),
        Load(R0, LabelOp(".L._printc_str0")),
        BranchAndLink("printf"),
        Move(R0, ImmVal(0)),
        BranchAndLink("fflush"),
        Pop(List(PC))
      )
    )

  }

  private def printBool()(implicit
      instructions: mutable.ListBuffer[Instruction]
  ): Unit = {
    instructions.addAll(
      List(
        Directive("data"),
        Directive("    word 5"),
        Label(".L._printb_str0"),
        Directive(s"    asciz \"false\""),
        Directive("    word 4"),
        Label(".L._printb_str1"),
        Directive(s"    asciz \"true\""),
        Directive("    word 4"),
        Label(".L._printb_str2"),
        Directive(s"    asciz \"%.*s\""),
        Directive("text"),
        Label("_printb"),
        Push(List(LR)),
        Cmp(R0, ImmVal(0)),
        Branch(".L_printb0", NE),
        Load(R2, LabelOp(".L._printb_str0")),
        Branch(".L_printb1"),
        Label(".L_printb0"),
        Load(R2, LabelOp(".L._printb_str1")),
        Label(".L_printb1"),
        Load(R1, PreIndexedMode(R2, shiftAmount = ImmVal(-WORD_SIZE))),
        Load(R0, LabelOp(".L._printb_str2")),
        BranchAndLink("printf"),
        Move(R0, ImmVal(0)),
        BranchAndLink("fflush"),
        Pop(List(PC))
      )
    )

  }

  private def printLine()(implicit
      instructions: mutable.ListBuffer[Instruction]
  ): Unit = {
    instructions.addAll(
      List(
        Directive("data"),
        Directive("    word 0"),
        Label(".L._println_str0"),
        Directive(s"    asciz \"\""),
        Directive("text"),
        Label("_println"),
        Push(List(LR)),
        Load(R0, LabelOp(".L._println_str0")),
        BranchAndLink("puts"),
        Move(R0, ImmVal(0)),
        BranchAndLink("fflush"),
        Pop(List(PC))
      )
    )
  }

  private def printP()(implicit
      instructions: mutable.ListBuffer[Instruction]
  ): Unit = {
    instructions.addAll(
      List(
        Directive("data"),
        Directive("    word 2"),
        Label(".L._printp_str0"),
        Directive(s"    asciz \"%p\""),
        Directive("text"),
        Label("_printp"),
        Push(List(LR)),
        Move(R1, R0),
        Load(R0, LabelOp(".L._printp_str0")),
        BranchAndLink("printf"),
        Move(R0, ImmVal(0)),
        BranchAndLink("fflush"),
        Pop(List(PC))
      )
    )
  }

  private def readInt()(implicit
      instructions: mutable.ListBuffer[Instruction]
  ): Unit = {
    instructions.addAll(
      List(
        Directive("data"),
        Directive("    word 2"),
        Label(".L._readi_str0"),
        Directive(s"    asciz \"%d\""),
        Directive("text"),
        Label("_readi"),
        Push(List(LR)),
        // str r0 [sp, #-4]!
        Store(R0, PostIndexedMode(SP, shiftAmount = ImmVal(-WORD_SIZE))),
        Move(R1, SP),
        // ldr r0, .L._readi_str0
        Load(R0, LabelOp(".L._readi_str0")),
        BranchAndLink("scanf"),
        // ldr r0, [sp, #0]
        Load(R0, OffsetMode(SP, shiftAmount = ImmVal(0))),
        AddInstr(SP, SP, ImmVal(WORD_SIZE)),
        Pop(List(PC))
      )
    )
  }

  private def readChar()(implicit
      instructions: mutable.ListBuffer[Instruction]
  ): Unit = {
    instructions.addAll(
      List(
        Directive("data"),
        Directive("    word 2"),
        Label(".L._readc_str0"),
        Directive(s"    asciz \" %c\""),
        Directive("text"),
        Label("_readc"),
        Push(List(LR)),
        // str r0 [sp, #-4]!
        StoreByte(R0, PostIndexedMode(SP, shiftAmount = ImmVal(-CHAR_SIZE))),
        Move(R1, SP),
        // ldr r0, .L._readc_str0
        Load(R0, LabelOp(".L._readc_str0")),
        BranchAndLink("scanf"),
        LoadByte(R0, OffsetMode(SP, shiftAmount = ImmVal(0))),
        AddInstr(SP, SP, ImmVal(CHAR_SIZE)),
        Pop(List(PC))
      )
    )
  }

  private def intErrOverflow()(implicit
      instructions: mutable.ListBuffer[Instruction]
  ): Unit = {
    instructions.addAll(
      List(
        Directive("data"),
        Comment("length of .L._errOverflow_str0"),
        Directive("    word 52"),
        Label(".L._errOverflow_str0"),
        Directive(
          s"    asciz \"fatal error: integer overflow or underflow occurred\""
        ),
        Directive("text"),
        Label("_errOverflow"),
        // ldr r0, .L._errOverflow_str0
        Load(R0, LabelOp(".L._errOverflow_str0")),
        BranchAndLink("_prints"),
        Move(R0, ImmVal(255)),
        BranchAndLink("exit")
      )
    )
  }

  private def intErrDivZero()(implicit
      instructions: mutable.ListBuffer[Instruction]
  ): Unit = {
    instructions.addAll(
      List(
        Directive("data"),
        Comment("length of .L._errDivByZero_str0"),
        Directive("    word 39"),
        Label(".L._errDivByZero_str0"),
        Directive(s"    asciz \"fatal error: division or modulo by zero\""),
        Directive("text"),
        Label("_errDivByZero"),
        // ldr r0, .L._errDivByZero_str0
        Load(R0, LabelOp(".L._errDivByZero_str0")),
        BranchAndLink("_prints"),
        Move(R0, ImmVal(255)),
        BranchAndLink("exit")
      )
    )
  }

  private def freePair()(implicit
      instructions: mutable.ListBuffer[Instruction]
  ): Unit = {
    instructions.addAll(
      List(
        Directive("data"),
        Label("_freepair"),
        Push(List(LR)),
        Move(R8, R0),
        Cmp(R8, ImmVal(0)),
        Branch("_errNull", cond = Condition.EQ),
        // ldr r0, [r8, #0]
        Load(R0, OffsetMode(R8, shiftAmount = ImmVal(0))),
        BranchAndLink("free"),
        // ldr r0, [r8, #4]
        Load(R0, OffsetMode(R8, shiftAmount = ImmVal(WORD_SIZE))),
        BranchAndLink("free"),
        Move(R0, R8),
        BranchAndLink("free"),
        Pop(List(PC))
      )
    )
  }

  private def errNull()(implicit
      instructions: mutable.ListBuffer[Instruction]
  ): Unit = {
    instructions.addAll(
      List(
        Directive("data"),
        Directive("    word 44"),
        Label(".L._errNull_str0"),
        Directive(
          s"    asciz \"fatal error: null pair dereferenced or freed\""
        ),
        Directive("text"),
        Label("_errNull"),
        // ldr r0, .L._errNull_str0
        Load(R0, LabelOp(".L._errNull_str0")),
        BranchAndLink("_prints"),
        Move(R0, ImmVal(255)),
        BranchAndLink("exit")
      )
    )
  }

  private def arrStore()(implicit
      instructions: mutable.ListBuffer[Instruction]
  ): Unit = {
    instructions.addAll(
      List(
        /* @ Special calling convention: array ptr passed in R3, index in R10,
            value to store in R8, LR (R14) is used as general register */
        Label("_arrStore"),
        Push(List(LR)),
        Cmp(R10, ImmVal(0)),
        Move(R1, R10, Condition.LT),
        BranchAndLink("_boundsCheck", Condition.LT),
        Load(LR, OffsetMode(baseReg = R3, shiftAmount = ImmVal(-4))),
        Cmp(R10, LR),
        Move(R1, R10, Condition.GE),
        BranchAndLink("_boundsCheck", Condition.GE),
        // str r8, [r3, r10, lsl #2]
        StrShift(R8, R3, R10, ShiftType.LSL, ImmVal(2)),
        Pop(List(PC))
      )
    )
  }

  private def arrStoreB()(implicit
      instructions: mutable.ListBuffer[Instruction]
  ): Unit = {

    instructions.addAll(
      List(
        Label("_arrStoreB"),
        Push(List(LR)),
        Cmp(R10, ImmVal(0)),
        Move(R1, R10, Condition.LT),
        BranchAndLink("_boundsCheck", Condition.LT),
        Load(LR, OffsetMode(baseReg = R3, shiftAmount = ImmVal(-4))),
        Cmp(R10, LR),
        Move(R1, R10, Condition.GE),
        BranchAndLink("_boundsCheck", Condition.GE),
        // strb r8, [r3, r10]
        StoreByte(R8, PreIndexedMode(baseReg = R3, auxReg = Some(R10))),
        Pop(List(PC))
      )
    )

  }

  private def arrLoad()(implicit
      instructions: mutable.ListBuffer[Instruction]
  ): Unit = {
    instructions.addAll(
      List(
        Label("_arrLoad"),
        Push(List(LR)),
        Cmp(R10, ImmVal(0)),
        Move(R1, R10, Condition.LT),
        BranchAndLink("_boundsCheck", Condition.LT),
        Load(LR, OffsetMode(baseReg = R3, shiftAmount = ImmVal(-4))),
        Cmp(R10, LR),
        Move(R1, R10, Condition.GE),
        BranchAndLink("_boundsCheck", Condition.GE),
        LdrShift(
          R3,
          R3,
          R10,
          ShiftType.LSL,
          ImmVal(2)
        ), // ldr r3, [r3, r10, lsl #2]
        Pop(List(PC))
      )
    )
  }

  private def boundsCheck()(implicit
      instructions: mutable.ListBuffer[Instruction]
  ): Unit = {
    instructions.addAll(
      List(
        Directive("data"),
        Directive("    word 41"),
        Label(".L._boundsCheck_str0"),
        Directive(s"    asciz \"fatal error: array index %d out of bounds\""),
        Directive("text"),
        Label("_boundsCheck"),
        Load(R0, LabelOp(".L._boundsCheck_str0")),
        BranchAndLink("printf"),
        Move(R0, ImmVal(0)),
        BranchAndLink("fflush"),
        Move(R0, ImmVal(255)),
        BranchAndLink("exit")
      )
    )
  }

}
