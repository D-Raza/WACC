package wacc.backend

sealed trait Register extends Operand2 {
  def n: Int
}
case object R1 extends Register { def n = 1 }
case object R2 extends Register { def n = 2 }
case object R3 extends Register { def n = 3 }
case object R4 extends Register { def n = 4 }
case object R5 extends Register { def n = 5 }
case object R6 extends Register { def n = 6 }
case object R7 extends Register { def n = 7 }
case object R8 extends Register { def n = 8 }
case object R9 extends Register { def n = 9 }
case object R10 extends Register { def n = 10 }
case object R11 extends Register { def n = 11 }
case object R12 extends Register { def n = 12 }
case object StackPointer extends Register { def n = 13 }
case object LinkRegister extends Register { def n = 14 }
case object ProgramCounter extends Register { def n = 15 }
