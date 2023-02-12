package wacc.backend
import Condition._
import Flag._

object Instruction {
  // Arithmetic instructions
  case class AddInstr(
      destReg: Register,
      operand1: Register,
      operand2: Operand2,
      cond: Condition,
      flag: Flag
  )

  case class SubInstr(
      destReg: Register,
      operand1: Register,
      operand2: Operand2,
      cond: Condition,
      flag: Flag
  )

  case class Rsb(
      destReg: Register,
      operand1: Register,
      operand2: Operand2,
      cond: Condition
  )

  case class SMull(
      rdLo: Register,
      rdHi: Register,
      rn: Register,
      rm: Register,
      cond: Condition
  )

  // Logical instructions
  case class AndInstr(
      destReg: Register,
      operand1: Register,
      operand2: Operand2,
      cond: Condition
  )

  case class OrrInstr(
      destReg: Register,
      operand1: Register,
      operand2: Operand2,
      cond: Condition
  )

  case class EorInstr(
      destReg: Register,
      operand1: Register,
      operand2: Operand2,
      cond: Condition
  )

  // Data transfer instructions
  case class Move(destReg: Register, operand: Operand2, cond: Condition)
  case class Store(srcReg: Register, dest: Operand2, cond: Condition)
  case class Load(destReg: Register, operand: Operand2, cond: Condition)
  case class Pop(srcReg: Register, cond: Condition)
  case class Push(destReg: Register, cond: Condition)

  // Label instructions
  case class Label(label: String)
  case class Directive(directiveType: String)

  // Branch instructions
  case class Branch(label: String, cond: Condition)
  case class BranchAndLink(label: String, cond: Condition)

  // Compare instruction
  case class Cmp(srcReg: Register, opearand: Operand2, cond: Condition)
}
