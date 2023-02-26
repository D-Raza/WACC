package wacc.backend
import wacc.backend.Globals.{PAIR_SIZE, WORD_SIZE}

import scala.collection.mutable

object Utils {

    def printString()(implicit instructions: mutable.ListBuffer[Instruction]): Unit = {
        val stringDataFormat = "%.*s\u0000"
        instructions.addAll(
            List(
                Label("p_print_string"),
                Push(List(LR)),
                Load(R1, R0),
                AddInstr(R2, R0, ImmVal(WORD_SIZE)),
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
                // add int data format to data label
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

    }
}
