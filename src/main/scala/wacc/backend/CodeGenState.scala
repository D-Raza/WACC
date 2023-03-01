package wacc.backend

import wacc.AST.Ident
import scala.collection.mutable

case class CodeGenState() {
  private val labelIdGenerator: Iterator[Int] = Iterator.from(0)
  def getNewLabelId: Int = labelIdGenerator.next()

  val scratchRegs: mutable.Stack[Register] =
    mutable.Stack(R4, R5, R6, R7, R0, R1, R2, R3)
  val tmp: Register = R8
  val tmp2: Register = IP

  def getScratchReg: Option[Register] =
    if (scratchRegs.isEmpty) None else Some(scratchRegs.pop())

  var recentReg: Option[Register] = None
  var secondRecentReg: Option[Register] = None

  def updateRecentRegs(re1: Register, re2: Register): Unit = {
    secondRecentReg = Some(re2)
    recentReg = Some(re1)
  }

  val identToReg: mutable.Map[Ident, Register] = mutable.Map.empty

}

/*
sealed trait InsArg
case class Register1(r: Int) extends InsArg with (RegResolution => Register1) {
  def apply(r: RegResolution): Register1 = this
}
case class RegResolution(state: CodeGenState, re2: Register1, re1: Register1, reNew: Register1)
sealed trait IndefReg extends (RegResolution => Register)

// The register most recently written to
case object Re1 extends IndefReg {
  def apply(r: RegResolution): Register1 = r.re1
}

// The register written to before the most recent write
case object Re2 extends IndefReg {
  def apply(r: RegResolution): Register1 = r.re2
}


object X {
  def resolveInstr4[A <: InsArg, B <: InsArg, C <: InsArg, D <: InsArg]
                   (state: CodeGenState, instr: (A, B, C, D) => List[Instruction])
                   (argA: RegResolution => A, argB: RegResolution => B, argC: RegResolution => C, argD: RegResolution => D)
                   (out: IndefReg): List[Instruction] = {

                    val args = (argA, argB, argC, argD)

                    List.empty
                   }
}
 */
