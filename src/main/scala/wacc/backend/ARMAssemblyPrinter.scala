package wacc.backend

object ARMAssemblyPrinter {
  def printAsm(asm: List[Instruction]): String =
    asm.map(printInstr).mkString("\n")

  def printInstr(instr: Instruction): String = instr match {
    // Arithmetic instructions
    // TODO: Handle signed and unsigned flags
    case AddInstr(destReg, operand1, operand2, cond, flag) =>
      s"    add${cond.toString}${flag.map(f => s".$f").getOrElse("")} ${printReg(
          destReg
        )}, ${printReg(operand1)}, ${printOperand2(operand2)}"
    case SubInstr(destReg, operand1, operand2, cond, flag) =>
      s"    sub${cond.toString}${flag.map(f => s".$f").getOrElse("")} ${printReg(
          destReg
        )}, ${printReg(operand1)}, ${printOperand2(operand2)}"
    case Rsb(destReg, operand1, operand2, cond) =>
      s"    rsb${cond.toString} ${printReg(destReg)}, ${printReg(operand1)}, ${printOperand2(operand2)}"
    case SMull(rdLo, rdHi, rn, rm, cond) =>
      s"    smull${cond.toString} ${rdLo.toString}, ${rdHi.toString}, ${rn.toString}, ${rm.toString}"
    case AndInstr(destReg, operand1, operand2, cond) =>
      s"    and${cond.toString} ${printReg(destReg)}, ${printReg(operand1)}, ${printOperand2(operand2)}"
    case OrrInstr(destReg, operand1, operand2, cond) =>
      s"    orr${cond.toString} ${printReg(destReg)}, ${printReg(operand1)}, ${printOperand2(operand2)}"
    case EorInstr(destReg, operand1, operand2, cond) =>
      s"    eor${cond.toString} ${printReg(destReg)}, ${printReg(operand1)}, ${printOperand2(operand2)}"
    case XorInstr(destReg, operand1, operand2, cond) =>
      s"    xor${cond.toString} ${printReg(destReg)}, ${printReg(operand1)}, ${printOperand2(operand2)}"

    // Data transfer instructions
    case Move(destReg, operand2, cond) =>
      s"    mov${cond.toString} ${printReg(destReg)}, ${printOperand2(operand2)}"
    case Store(destReg, operand2, cond) =>
      s"    str${cond.toString} ${printReg(destReg)}, ${printOperand2(operand2)}"
    case Load(destReg, operand2, cond) =>
      s"    ldr${cond.toString} ${printReg(destReg)}, ${printOperand2(operand2)}"
    case Pop(srcRegs, cond) =>
      s"    pop${cond.toString} {${srcRegs.map(printReg).mkString(", ")}}"
    case Push(destRegs, cond) =>
      s"    push${cond.toString} {${destRegs.map(printReg).mkString(", ")}}"

    // Label instructions
    case Label(label)             => s"$label:"
    case Directive(directiveType) => s".$directiveType"

    // Branch instructions
    case Branch(label, cond)         => s"    b${cond.toString} $label"
    case BranchAndLink(label, cond)  => s"    bl${cond.toString} $label"
    case BranchAndLinkReg(reg, cond) => s"    blx${cond.toString} $reg"

    // Compare instruction
    case Cmp(operand1, operand2, cond) =>
      s"    cmp${cond.toString} ${printReg(operand1)}, ${printOperand2(operand2)}"
  }

  def printOperand2(operand2: Operand2): String = operand2 match {
    case ImmVal(x)           => s"#$x"
    case ImmChar(c)          => s"#\'$c\'"
    case LoadImmVal(x)       => s"=$x"
    case reg: Register       => printReg(reg)
    case op2: AddressingMode => printAddressingMode(op2)
  }

  def printAddressingMode(op2: AddressingMode): String = op2 match {
    case OffsetMode(baseReg, auxReg, shiftType, shiftAmount) =>
      s"[${printReg(baseReg)}${auxReg.map(r => s", ${printReg(r)}").getOrElse("")}${shiftType
          .map(s => s", ${s.toString}")
          .getOrElse("")}${printOperand2(shiftAmount)}]"
    case PreIndexedMode(baseReg, auxReg, shiftType, shiftAmount) =>
      s"[${printReg(baseReg)}${auxReg.map(r => s", ${printReg(r)}").getOrElse("")}${shiftType
          .map(s => s", ${s.toString}")
          .getOrElse("")}${printOperand2(shiftAmount)}]!"
    case PostIndexedMode(baseReg, auxReg, shiftType, shiftAmount) =>
      s"[${printReg(baseReg)}${auxReg.map(r => s", ${printReg(r)}").getOrElse("")}${shiftType
          .map(s => s", ${s.toString}")
          .getOrElse("")}${printOperand2(shiftAmount)}]"
    case _ => "@ TODO"
  }

  def printReg(reg: Register): String = reg match {
    case SP            => "sp"
    case LR            => "lr"
    case PC            => "pc"
    case FP            => "fp"
    case reg: Register => s"r${reg.n}"
  }
}
