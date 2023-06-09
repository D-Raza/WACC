package wacc.backend
import ShiftType._

trait Operand2 {
  def putOnStack(offset: Int): Operand2 = this match {
    case ImmVal(curOff) => ImmVal(-offset + curOff)
    case OffsetMode(baseReg, auxReg, shiftType, ImmVal(curOff)) =>
      OffsetMode(baseReg, auxReg, shiftType, ImmVal(-offset + curOff))
    case PostIndexedMode(baseReg, auxReg, shiftType, ImmVal(curOff)) =>
      PostIndexedMode(baseReg, auxReg, shiftType, ImmVal(-offset + curOff))
    case PreIndexedMode(baseReg, auxReg, shiftType, ImmVal(curOff)) =>
      PreIndexedMode(baseReg, auxReg, shiftType, ImmVal(-offset + curOff))
    case _ => this
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
        .getOrElse("")}${if (shiftAmount.value != 0) s", ${shiftAmount.toString()}"
      else ""}]"
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
) extends AddressingMode {
  override def toString: String = {
    s"[${baseReg.toString()}${auxReg.map(r => s", ${r.toString()}").getOrElse("")}${shiftType
        .map(s => s", ${s.toString}")
        .getOrElse("")}, ${shiftAmount.toString()}]!"
  }
}

case class PreIndexedMode(
    baseReg: Register,
    auxReg: Option[Register] = None,
    shiftType: Option[ShiftType] = None,
    shiftAmount: ImmVal = ImmVal(0)
) extends AddressingMode

// Immediate value
case class ImmVal(value: Int) extends Operand2 {
  override def toString: String = s"#$value"
}

// Immediate char
case class ImmChar(char: Char) extends Operand2 {
  override def toString: String = s"#\'$char\'"
}

// Load immediate value
case class LoadImmVal(value: Int) extends Operand2 {
  override def toString: String = s"=$value"
}

case class LabelOp(value: String) extends Operand2 {
  override def toString = s"=$value"
}
