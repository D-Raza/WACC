package wacc.backend

object Flag extends Enumeration {
  type Flag = Value

  // Condition flags: Negative (N), Zero (Z), Carry or Unsigned Overflow (C), Signed Overflow (V)
  val N, Z, C, V = Value
}
