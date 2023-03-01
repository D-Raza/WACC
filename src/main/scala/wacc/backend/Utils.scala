package wacc.backend
import wacc.backend._
import wacc.backend.Globals.WORD_SIZE
import wacc.backend.Condition.NE

import scala.collection.mutable

object Utils {

  var printStringFlag = false
  var printIntFlag = false
  var printCharFlag = false
  var printBoolFlag = false
  var printlnFlag = false
  var printRefFlag = false

  def addUtils()(implicit
      instructions: mutable.ListBuffer[Instruction]
  ): Unit = {
    if (printStringFlag) {
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
  }

  def printString()(implicit
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

  def printInt()(implicit
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

  def printChar()(implicit
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
  def printBool()(implicit
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

}
