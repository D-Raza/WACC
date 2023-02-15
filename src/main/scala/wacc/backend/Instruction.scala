package wacc.backend
import Condition._
import Flag._

trait Instruction

// Arithmetic instructions
case class AddInstr(
    destReg: Register,
    operand1: Register,
    operand2: Operand2,
    cond: Condition = AL,
    flag: Option[Flag] = None
) extends Instruction

case class SubInstr(
    destReg: Register,
    operand1: Register,
    operand2: Operand2,
    cond: Condition = AL,
    flag: Option[Flag] = None
) extends Instruction

case class Rsb(
    destReg: Register,
    operand1: Register,
    operand2: Operand2,
    cond: Condition
) extends Instruction

case class SMull(
    rdLo: Register,
    rdHi: Register,
    rn: Register,
    rm: Register,
    cond: Condition
) extends Instruction

// Logical instructions
case class AndInstr(
    destReg: Register,
    operand1: Register,
    operand2: Operand2,
    cond: Condition = AL
) extends Instruction

case class OrrInstr(
    destReg: Register,
    operand1: Register,
    operand2: Operand2,
    cond: Condition = AL
) extends Instruction

case class EorInstr(
    destReg: Register,
    operand1: Register,
    operand2: Operand2,
    cond: Condition = AL
) extends Instruction

// Data transfer instructions
case class Move(destReg: Register, operand: Operand2, cond: Condition = AL)
    extends Instruction
case class Store(srcReg: Register, dest: Operand2, cond: Condition = AL)
    extends Instruction
case class Load(destReg: Register, operand: Operand2, cond: Condition = AL)
    extends Instruction
case class Pop(srcReg: Register, cond: Condition = AL) extends Instruction
case class Push(destReg: Register, cond: Condition = AL) extends Instruction

// Label instructions
case class Label(label: String) extends Instruction
case class Directive(directiveType: String) extends Instruction

// Branch instructions
case class Branch(label: String, cond: Condition = AL) extends Instruction
case class BranchAndLink(label: String, cond: Condition = AL)
    extends Instruction
case class BranchAndLinkReg(reg: Register, cond: Condition = AL)
    extends Instruction

// Compare instruction
case class Cmp(srcReg: Register, opearand: Operand2, cond: Condition)
    extends Instruction
