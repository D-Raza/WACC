package wacc.backend
import ShiftType._

trait Operand2

// Addressing modes
sealed trait AddressingMode extends Operand2
case class OffsetMode(
    baseReg: Register,
    auxReg: Option[Register] = None,
    shiftType: Option[ShiftType] = None,
    shiftAmount: ImmVal = ImmVal(0)
) extends AddressingMode

case class PostIndexedMode(
    baseReg: Register,
    auxReg: Option[Register] = None,
    shiftType: Option[ShiftType] = None,
    shiftAmount: ImmVal = ImmVal(0)
) extends AddressingMode

case class PreIndexedMode(
    baseReg: Register,
    auxReg: Option[Register] = None,
    shiftType: Option[ShiftType] = None,
    shiftAmount: ImmVal = ImmVal(0)
) extends AddressingMode

// Immediate value
case class ImmVal(value: Int) extends Operand2

// Immediate char
case class ImmChar(char: Char) extends Operand2

// Load immediate value
case class LoadImmVal(value: Int) extends Operand2
