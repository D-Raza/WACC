package wacc.backend

import wacc.AST._
import scala.collection.mutable

object CodeGenerator {
  // Compiles the program
  def compileProgram(programNode: Program, codeGenState: CodeGeneratorState)(
      implicit instructions: mutable.ListBuffer[Instruction]
  ): CodeGeneratorState = {
    var newCodeGenState = codeGenState

    programNode.funcs.foreach(func =>
      newCodeGenState = newCodeGenState.addFunctionName(func.ident.name)
    )
    programNode.funcs.foreach(func => compileFunc(func, newCodeGenState))

    instructions.addAll(
      List(
        Label("main"),
        Push(LinkRegister)
      )
    )
    newCodeGenState = newCodeGenState.copy(stackPointerOffset =
      newCodeGenState.stackPointerOffset + 4
    )

    programNode.stat.foreach(stat =>
      newCodeGenState = compileStatWithNewScope(stat, newCodeGenState)
    )

    instructions.addAll(
      List(
        Load(R0, LoadImmVal(0)),
        Pop(ProgramCounter),
        Directive(".ltorg")
      )
    )

    newCodeGenState.copy(stackPointerOffset =
      newCodeGenState.stackPointerOffset - 4
    )
  }

  // Compiles function declaration
  def compileFunc(funcNode: Func, codeGenState: CodeGeneratorState)(implicit
      instructions: mutable.ListBuffer[Instruction]
  ): CodeGeneratorState = {
    var newCodeGenState = codeGenState

    funcNode.paramList.foreach(param =>
      newCodeGenState = compileParam(param, newCodeGenState)
    )
    instructions.addAll(
      List(
        Label("wacc_" + funcNode.ident.name),
        Push(LinkRegister)
      )
    )
    newCodeGenState = newCodeGenState.copy(stackPointerOffset =
      newCodeGenState.stackPointerOffset + 4
    )

    // Needed for restoring stack pointer after compiling the body of the function
    val newIdentToOffset =
      newCodeGenState.identToOffset + ("originalSP" -> newCodeGenState.stackPointerOffset)
    newCodeGenState = newCodeGenState.copy(identToOffset = newIdentToOffset)

    funcNode.stats.foreach(stat =>
      newCodeGenState = compileStatWithNewScope(stat, newCodeGenState)
    )

    newCodeGenState = newCodeGenState.copy(identToOffset =
      newCodeGenState.identToOffset - "originalSP"
    )

    instructions.addAll(
      List(
        Pop(ProgramCounter),
        Directive(".ltorg")
      )
    )

    val newStackPointerOffset =
      newCodeGenState.stackPointerOffset - newCodeGenState.usedStackSize - 4
    codeGenState.copy(
      usedStackSize = 0,
      stackPointerOffset = newStackPointerOffset
    )
  }

  // Compiles function parameter
  def compileParam(
      paramNode: Param,
      codeGenState: CodeGeneratorState
  ): CodeGeneratorState = {
    val newIdentToOffset =
      codeGenState.identToOffset + (paramNode.ident.name -> (paramNode.ty.size + codeGenState.stackPointerOffset))
    val newUsedStackSize = codeGenState.usedStackSize + paramNode.ty.size
    val newStackPointerOffset =
      codeGenState.stackPointerOffset + paramNode.ty.size

    codeGenState.copy(
      identToOffset = newIdentToOffset,
      usedStackSize = newUsedStackSize,
      stackPointerOffset = newStackPointerOffset
    )
  }

  def compileStatWithNewScope(statNode: Stat, codeGenState: CodeGeneratorState)(
      implicit instructions: mutable.ListBuffer[Instruction]
  ): CodeGeneratorState = {
    // TODO
    codeGenState
  }

}
