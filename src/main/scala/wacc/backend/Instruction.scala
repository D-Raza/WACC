package wacc.backend
import Condition._
import Flag._

sealed trait Instruction {
  def opsToString: String
  val opcode: String
  override def toString: String = s"\t$opcode $opsToString"
}

// Arithmetic instructions
case class AddInstr(
    destReg: Register,
    operand1: Register,
    operand2: Operand2,
    cond: Condition = AL,
    flag: Option[Flag] = None
) extends Instruction {
  override val opcode = "add"
  override def opsToString: String = s"$destReg, $operand1, $operand2"
}

case class SubInstr(
    destReg: Register,
    operand1: Register,
    operand2: Operand2,
    cond: Condition = AL,
    flag: Option[Flag] = None
) extends Instruction {
  override val opcode: String = "sub"
  override def opsToString: String = s"$destReg, $operand1, $operand2"

}

case class Rsb(
    destReg: Register,
    operand1: Register,
    operand2: Operand2,
    cond: Condition = AL
) extends Instruction {
  override val opcode: String = "rsb"
  override def opsToString: String = s"$destReg, $operand1, $operand2"
}

case class SMull(
    rdLo: Register,
    rdHi: Register,
    rn: Register,
    rm: Register,
    cond: Condition = AL
) extends Instruction {

  override val opcode: String = "smull"
  override def opsToString: String = s"$rdLo, $rdHi, $rn, $rm"

}

// Logical instructions
case class AndInstr(
    destReg: Register,
    operand1: Register,
    operand2: Operand2,
    cond: Condition = AL
) extends Instruction {
  override val opcode = "and"
  override def opsToString: String = s"$destReg, $operand1, $operand2"
}

case class OrrInstr(
    destReg: Register,
    operand1: Register,
    operand2: Operand2,
    cond: Condition = AL
) extends Instruction {
  override val opcode = "orr"
  override def opsToString: String = s"$destReg, $operand1, $operand2"
}

case class XorInstr(
    destReg: Register,
    operand1: Register,
    operand2: Operand2,
    cond: Condition = AL
) extends Instruction {
  override val opcode = "eor" // not XOR
  override def opsToString: String = s"$destReg, $operand1, $operand2"
}

// Data transfer instructions
case class Move(destReg: Register, operand: Operand2, cond: Condition = AL)
    extends Instruction {
  override val opcode = "mov"
  override def opsToString: String = s"$destReg, $operand"
}

case class Store(srcReg: Register, dest: Operand2, cond: Condition = AL)
    extends Instruction {
  override val opcode = "str"
  override def opsToString: String = s"$srcReg, $dest"
}

case class Load(destReg: Register, operand: Operand2, cond: Condition = AL)
    extends Instruction {
  override val opcode = "ldr"
  override def opsToString: String = s"$destReg, $operand"
}

case class Pop(srcRegs: List[Register], cond: Condition = AL)
    extends Instruction {
  val opcode: String = "pop"
  override def opsToString: String =
    "{" + srcRegs.map(_.toString).mkString(", ") + "}"
}

case class Push(destRegs: List[Register], cond: Condition = AL)
    extends Instruction {
  val opcode = "push"
  override def opsToString: String =
    "{" + destRegs.map(_.toString).mkString(", ") + "}"
}

// Label instructions
case class Label(label: String) extends Instruction {
  override def opsToString: String = ""
  val opcode = ""
  override def toString = label + ":"
}

case class Directive(directiveType: String) extends Instruction {
  override def opsToString: String = ""
  val opcode = ""
  override def toString = "." + directiveType
}

// Branch instructions
case class Branch(label: String, cond: Condition = AL) extends Instruction {
  override def opsToString = label
  override val opcode = "b"
}
case class BranchAndLink(label: String, cond: Condition = AL)
    extends Instruction {
  override def opsToString = label
  override val opcode = "bl"
}
case class BranchAndLinkReg(reg: Register, cond: Condition = AL)
    extends Instruction {
  override def opsToString: String = reg.toString()
  override val opcode = "blx"
}

// Compare instruction
case class Cmp(srcReg: Register, operand: Operand2, cond: Condition = AL)
    extends Instruction {
  override def opsToString: String = s"$srcReg, $operand"
  override val opcode = "cmp"
}
