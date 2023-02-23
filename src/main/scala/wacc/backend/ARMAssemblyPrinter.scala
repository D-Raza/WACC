package wacc.backend

object ARMAssemblyPrinter {
  def printAsm(asm: List[Instruction]): String =
    asm.map(_.toString).mkString("\n")
}
