package wacc.backend

sealed trait Register extends Operand2 {
  def n: Int
  override def toString: String = s"r$n"
}

object Globals {
  val BYTE_SIZE = 1
  val CHAR_SIZE = 1
  val WORD_SIZE = 4
  val PAIR_SIZE = 8
}

case object R0 extends Register { def n = 0 }
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

// Frame/base pointer
case object FP extends Register {
  def n = 11
  override def toString: String = "fp"
}

// Intra-procedure call scratch register
case object IP extends Register {
  def n = 12
  override def toString: String = "ip"
}

// Stack pointer
case object SP extends Register {
  def n = 13
  override def toString: String = "sp"
}

// Link register
case object LR extends Register {
  def n = 14
  override def toString: String = "lr"
}

// Program counter
case object PC extends Register {
  def n = 15
  override def toString: String = "pc"
}
