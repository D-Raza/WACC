package wacc.backend

import scala.collection.mutable
import wacc.AST.{Param, Ident, Type}

class StackMachine {
  val stackFrameList: mutable.ListBuffer[StackFrame] = mutable.ListBuffer.empty
  def currStackSize =
    if (stackFrameList.nonEmpty) stackFrameList.last.currVarOffset else 0

  def addDeclaration(ident: Ident, size: Int): Int = {
    val currStackFrame = stackFrameList.last
    currStackFrame.declaredVarMap += (ident -> currStackFrame.currVarOffset)

    currStackFrame.currVarOffset += size

    currStackFrame.currVarOffset - size
  }

  def addStackFrame(
      symbolTable: Map[Ident, Type],
      paramList: List[Param] = List(),
      isFun: Boolean = false
  ): mutable.ListBuffer[Instruction] = {
    val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer.empty

    val stackFrameToAdd = new StackFrame(symbolTable, paramList)

    if (isFun)
      stackFrameToAdd.currVarOffset =
        -24 // FP, LR, R4, R5, R6, R7 pushed on stack at start

    stackFrameList += stackFrameToAdd

    if (!isFun && stackFrameList.length == 1) {
      instructions.addAll(
        List(
          Push(List(FP, LR)),
          Push(List(R4, R5, R6, R7, R8, R10, IP)),
          Move(FP, SP)
        )
      )
    } else {
      instructions ++= (
        List(
          Push(List(FP)),
          Move(FP, SP)
        )
      )
    }

    instructions.addAll(assignStackSpace(stackFrameToAdd.currVarOffset))

    instructions
  }

  def removeStackFrame(
      isFun: Boolean = false
  ): mutable.ListBuffer[Instruction] = {
    val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer.empty

    val stackFrameToRemove = stackFrameList.remove(stackFrameList.length - 1)

    instructions.addAll(
      if (!isFun) unassignStackSpace(stackFrameToRemove.currVarOffset)
      else List()
    )
    if (stackFrameList.length == 0) {
      instructions ++= List(
        Pop(List(R4, R5, R6, R7, R8, R10, IP)),
        Pop(List(FP))
      )
    } else {
      instructions += Pop(List(FP))
    }

    instructions
  }

  private def assignStackSpace(
      spaceRequired: Int
  ): mutable.ListBuffer[Instruction] = {
    if (spaceRequired == 0)
      mutable.ListBuffer(
        PendingStackOffset(SubInstr(SP, SP, ImmVal(0)), stackFrameList.last)
      )
    else {
      val instructions: mutable.ListBuffer[Instruction] =
        mutable.ListBuffer.empty

      instructions += PendingStackOffset(
        SubInstr(SP, SP, ImmVal(spaceRequired)),
        stackFrameList.last
      )

      instructions
    }
  }

  private def unassignStackSpace(
      spaceAcquired: Int
  ): mutable.ListBuffer[Instruction] = {
    if (spaceAcquired == 0)
      mutable.ListBuffer.empty
    else {
      val instructions: mutable.ListBuffer[Instruction] =
        mutable.ListBuffer.empty

      if (spaceAcquired > 0)
        instructions += AddInstr(SP, SP, ImmVal(spaceAcquired))

      instructions
    }
  }

  def getIdentOffset(id: Ident): Int = {
    var found = false
    var res = 0

    for (stackFrame <- stackFrameList.reverse) {
      if (!found) {
        stackFrame.getVar(id) match {
          case Some(x) => {
            res = x
            found = true
          }
          case None => res = -999
        }
      }
    }
    res
  }
}

class StackFrame(symbolTable: Map[Ident, Type], paramList: List[Param]) {
  var declaredVarMap: Map[Ident, Int] = {
    var map: Map[Ident, Int] = Map.empty

    // the first four arguments are passed in R0-R3, the rest on the stack
    var offset = 0
    for (param <- paramList.drop(4)) {
      map += (param.ident -> offset)
      offset += param.ty.size
    }

    map
  }

  var currVarOffset = 0
  var currArgOffset = 0

  def getVar(id: Ident): Option[Int] = declaredVarMap.get(id)
}
