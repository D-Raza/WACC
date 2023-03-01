// package wacc.backend

// import wacc.AST.Ident
// import scala.collection.mutable

// case class CodeGeneratorStateOld(
//     // Maps identifiers to registers
//     identToReg: Map[Ident, Register] = Map.empty,
//     // Maps identifiers to where they are stored on the stack relative to beginning of the stack
//     instructionIdxToSize: mutable.LinkedHashMap[Int, Int] =
//       mutable.LinkedHashMap.empty,
//     // Amount of stack space used for storing variables during compilation, needed for resetting the stack pointer to its original value upon exiting a scope
//     usedStackSize: Int = 0,
//     // Stores the original value of the stack pointer
//     originalSP: Int = 0,
//     // Distance between stack pointer and beginning of stack
//     stackPointerOffset: Int = 0,
//     // Stores the names of functions declared so far
//     declaredFunctionNames: List[String] = List.empty,
//     scratchRegs: mutable.Stack[Register] =
//       mutable.Stack(R4, R5, R6, R7, R0, R1, R2, R3),
//     tmp: Register = R8,
//     fallbackReg: Register = R4,
//     fallbackReg2: Register = R5,
//     lastResultReg: Register = R0
// ) {

//   // Starting from 0, generates label ids to be used in naming labels for branching
//   val labelIdGenerator: Iterator[Int] = Iterator.from(0)

//   def getScratchReg: Option[Register] =
//     if (scratchRegs.isEmpty) None else Some(scratchRegs.pop())

//   def getNewLabelId: Int = labelIdGenerator.next()

//   def addFunctionName(funcName: String): CodeGeneratorStateOld =
//     this.copy(declaredFunctionNames = declaredFunctionNames :+ funcName)

//   def stateForNewScope: CodeGeneratorStateOld =
//     this.copy(scratchRegs = mutable.Stack(R4, R5, R6, R7, R0, R1, R2, R3))

//   def getIdentReg(ident: Ident): Option[Register] = identToReg.get(ident)
// }
