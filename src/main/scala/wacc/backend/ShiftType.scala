package wacc.backend

object ShiftType extends Enumeration {
  type ShiftType = Value

  /* Right Arithmetic Shift (ASR), Left Arithmetic Shift (ASL),
       Left Logical Shift (LSL), Right Logical Shift (LSR), Rotate Right (ROR) */
  val ASR = Value("asr")
  val ASL = Value("asl")
  val LSL = Value("lsl")
  val LSR = Value("lsr")
  val ROR = Value("ror")

}
