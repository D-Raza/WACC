package wacc.backend

object Condition extends Enumeration {
  type Condition = Value

  // Condition codes
  val EQ = Value("eq")
  val NE = Value("ne")
  val GT = Value("gt")
  val LT = Value("lt")
  val GE = Value("ge")
  val LE = Value("le")
  val LS = Value("ls")
  val HI = Value("hi")
  val VC = Value("vc")
  val VS = Value("vs")
  val PL = Value("pl")
  val MI = Value("mi")
  val CC = Value("cc")
  val CS = Value("cs")
  val AL = Value("")
}
