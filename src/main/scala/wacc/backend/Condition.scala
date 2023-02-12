package wacc.backend

object Condition extends Enumeration {
  type Condition = Value

  // Condition codes
  val EQ, NE, AL, GT, LT, GE, LE, LS, HI, VC, VS, PL, MI, CC, CS = Value
}
