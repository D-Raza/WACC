package wacc.backend
import ShiftType._

trait Operand2 {
  override def toString: String = {
    this match {
      case ImmVal(x)           => s"#$x"
      case ImmChar(c)          => s"#\'$c\'"
      case LoadImmVal(x)       => s"=$x"
      case adm: AddressingMode => adm.toString()
      case reg: Register       => reg.toString()
    }
  }
}

// Addressing modes
sealed trait AddressingMode extends Operand2 {
  val baseReg: Register
  val auxReg: Option[Register]
  val shiftType: Option[ShiftType]
  val shiftAmount: ImmVal
  override def toString: String = {
    s"[${baseReg.toString()}${auxReg.map(r => s", ${r.toString()}").getOrElse("")}${shiftType
        .map(s => s", ${s.toString}")
        .getOrElse("")}${shiftAmount.toString()}]"
  }
}

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
