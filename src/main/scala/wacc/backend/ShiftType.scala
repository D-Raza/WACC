package wacc.backend

object ShiftType extends Enumeration {
  type ShiftType = Value

  /* Right Arithmetic Shift (ASR), Left Arithmetic Shift (ASL),
       Left Logical Shift (LSL), Right Logical Shift (LSR), Rotate Right (ROR) */
  val ASR, ASL, LSL, LSR, ROR = Value
}
