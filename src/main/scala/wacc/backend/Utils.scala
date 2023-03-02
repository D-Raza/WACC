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
  var readIntFlag = false
  var readCharFlag = false
  var intErrOverflowFlag = false
  var intErrDivZeroFlag = false

  def addUtils()(implicit
      instructions: mutable.ListBuffer[Instruction]
  ): Unit = {
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
    if (readIntFlag) {
      readInt()
    }
    if (readCharFlag) {
      readChar()
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
          s"    asciz \"fatal error: integer overflow or underflow occurred\n\""
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
        Directive("    word 40"),
        Label(".L._errDivByZero_str0"),
        Directive(
          s"    asciz \"fatal error: division or modulo by zero occurred\n\""
        ),
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

}
