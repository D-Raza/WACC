package wacc.backend
import scala.collection.immutable

case class CodeGeneratorState(
    // Maps identifiers to where they are stored on the stack relative to beginning of the stack
    identToOffset: immutable.Map[String, Int] = immutable.Map.empty,
    // Amount of stack space used for storing variables during compilation, needed for resetting the stack pointer to its original value upon exiting a scope
    usedStackSize: Int = 0,
    // Distance between stack pointer and beginning of stack
    stackPointerOffset: Int = 0,
    // Stores the names of functions declared so far
    declaredFunctionNames: List[String] = immutable.List.empty,
    // List of free registers for compilation
    availableRegs: List[Register] =
      immutable.List(R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, FP, IP)
) {

  // Starting from 0, generates label ids to be used in naming labels for branching
  val labelIdGenerator: Iterator[Int] = Iterator.from(0)

  def getResReg: Register = availableRegs.head

  def getNonResReg: Register = availableRegs.tail.head

  def getNewLabelId: Int = labelIdGenerator.next()

  def addFunctionName(funcName: String): CodeGeneratorState =
    this.copy(declaredFunctionNames = declaredFunctionNames :+ funcName)

  def stateForNewScope: CodeGeneratorState = this.copy()

  def getIdentOffset(identName: String): Int =
    identToOffset.getOrElse(identName, 0)
}
