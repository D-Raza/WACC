package wacc.backend

import wacc.AST._
import scala.collection._

// highestReg is the highest register that has been used so far

case class CodeGenState() {
  private val labelIdGenerator: Iterator[Int] = Iterator.from(0)
  def getNewLabelId: Int = labelIdGenerator.next()

  var availableRegs: List[Register] = List(
    R4,
    R5,
    R6,
    R7,
    R0,
    R1,
    R2,
    R3
  ) // R9, R10, R4, R5, R6, R7, R0, R1, R2, R3, R8)

  var usedRegs: List[Register] = List.empty

  def getReg: Register = {
    if (availableRegs.isEmpty) {
      // usedRegs should contain all registers
      // free a register from usedRegs
      val reg = usedRegs.last
      usedRegs = usedRegs.dropRight(1)
      usedRegs = reg +: usedRegs
      reg
    } else {
      val reg = availableRegs.head
      availableRegs = availableRegs.tail
      usedRegs = reg +: usedRegs
      reg
    }
  }

  def getRegOrNone: Option[Register] = {
    if (availableRegs.isEmpty) None
    else {
      // free a register from usedRegs, and add it to availableRegs
      val reg = availableRegs.head
      availableRegs = availableRegs.tail
      usedRegs = reg +: usedRegs
      Some(reg)
    }
  }

  val scratchRegs: mutable.Stack[Register] =
    mutable.Stack(R4, R5, R6, R7, R0, R1, R2, R3)
  val tmp: Register = R8
  val tmp2: Register = R9
  val tmp3: Register = IP

  var exitLabel: String = "main_exit"

  val identToReg: mutable.Map[Ident, Register] = mutable.Map.empty

}
