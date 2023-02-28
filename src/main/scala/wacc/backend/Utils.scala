package wacc.backend
import wacc.backend._
import wacc.backend.Globals.WORD_SIZE
import wacc.backend.Condition.{EQ, NE}

import scala.collection.mutable

object Utils {

    var printStringFlag = false
    var printIntFlag = false
    var printCharFlag = false
    var printBoolFlag = false
    var printlnFlag = false 
    var printRefFlag = false 


     def addUtils()(implicit instructions: mutable.ListBuffer[Instruction]): Unit = {
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
        if (printRefFlag) {
            printRef()
        }
    }

    def printString()(implicit instructions: mutable.ListBuffer[Instruction]): Unit = {
        val stringDataFormat = "%.*s\u0000"
        instructions.addAll(
            List(
                Label("p_print_string"),
                Push(List(LR)),
                Load(R1, R0),
                AddInstr(R2, R0, ImmVal(WORD_SIZE)),
                Load(R0, LabelOp(Labels.addDataMsg(stringDataFormat))),
                // TODO: Add string data format to data label
                BranchAndLink("printf"),
                Move(R0, ImmVal(0)),
                BranchAndLink("fflush"),
                Pop(List(PC))
            )
        )
    }

    def printInt()(implicit instructions: mutable.ListBuffer[Instruction]): Unit = {
        val intDataFormat = "%d\u0000"
        instructions.addAll(
            List(
                Label("p_print_int"),
                Push(List(LR)),
                Move(R1, R0),
                Load(R0, LabelOp(Labels.addDataMsg(intDataFormat))),
                BranchAndLink("printf"),
                Move(R0, ImmVal(0)),
                BranchAndLink("fflush"),
                Pop(List(PC))
            )
        )
    }

    def printChar()(implicit instructions: mutable.ListBuffer[Instruction]): Unit = {
        instructions.addAll(
            List(
                Label("p_print_char"),
                Push(List(LR)),
                BranchAndLink("putchar"),
                Pop(List(PC))
            )
        )
    }

    def printBool()(implicit instructions: mutable.ListBuffer[Instruction]): Unit = {
        val trueDataFormat = "true\u0000"
        val falseDataFormat = "false\u0000"
        instructions.addAll(
            List(
                Label("p_print_bool"),
                Push(List(LR)),
                Cmp(R0, ImmVal(0)),
                Load(R0, LabelOp(Labels.addDataMsg(trueDataFormat)), NE),
                Load(R1, LabelOp(Labels.addDataMsg(falseDataFormat)), EQ),
                BranchAndLink("printf"),
                Move(R0, ImmVal(0)),
                BranchAndLink("fflush"),
                Pop(List(PC))
            )
        )

    }



    private def printLine()(implicit instructions: mutable.ListBuffer[Instruction]): Unit = {
        instructions.addAll(
            List(
                Label("p_print_ln"),
                Push(List(LR)),
                Load(R0, LabelOp(Labels.addDataMsg("\n"))),
                BranchAndLink("printf"), // puts?
                Move(R0, ImmVal(0)),
                BranchAndLink("fflush"),
                Pop(List(PC))
            )
        )
    }

    def printRef()(implicit instructions: mutable.Buffer[Instruction]): Unit = {
        instructions.addAll(
            List(
                Label("p_print_reference"),
                Push(List(LR)),
                Move(R1, R0),
                Load(R0, LabelOp(Labels.addDataMsg("%p\u0000"))),

                AddInstr(R0, R0, ImmVal(WORD_SIZE)),
                BranchAndLink("printf"),
                Move(R0, ImmVal(0)),
                BranchAndLink("fflush"),
                Pop(List(PC))
            )
        )
    } 


   
}
