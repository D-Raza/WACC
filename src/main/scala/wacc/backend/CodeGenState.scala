package wacc.backend

import wacc.AST.Ident
import scala.collection._

// highestReg is the highest register that has been used so far

case class CodeGenState() {
  private val labelIdGenerator: Iterator[Int] = Iterator.from(0)
  def getNewLabelId: Int = labelIdGenerator.next()

  var availableRegs: List[Register] = List(R9, R10, R4, R5, R6, R7, R0, R1, R2, R3, R8)

  var usedRegs: List[Register] = List.empty

  def getReg: Register = {
    if (availableRegs.isEmpty) {
      // usedRegs should contain all registers 
      // free a register from usedRegs
      val reg = usedRegs.last
      usedRegs = usedRegs.dropRight(1)
      usedRegs = reg +: usedRegs
      reg
    }
    else {
      val reg = availableRegs.head
      availableRegs = availableRegs.tail
      usedRegs = reg +: usedRegs
      reg
    }
  }

  // Replacement for getScratchReg
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

  // Register most recently allocated to 
  val recentReg1 = R0

  // Register 2nd most recently allocated to 
  val recentReg2 = R0

  val scratchRegs: mutable.Stack[Register] =
    mutable.Stack(R4, R5, R6, R7, R0, R1, R2, R3)
  val tmp: Register = R8
  val tmp2: Register = IP

  def getScratchReg: Option[Register] =
    if (scratchRegs.isEmpty) None else Some(scratchRegs.pop())

  val identToReg: mutable.Map[Ident, Register] = mutable.Map.empty

}

