package wacc.backend

import scala.collection.mutable
import wacc.AST.{Param, Ident, Type}

object StackMachine {
  val stackFrameList: mutable.ListBuffer[StackFrame] = mutable.ListBuffer.empty
  var usedStackSize = 0

  def addDeclaration(ident: Ident, size: Int): Unit = {
    val currStackFrame = stackFrameList.last
    currStackFrame.declaredVarMap += (ident -> currStackFrame.currVarOffset)

    currStackFrame.currArgOffset += size
  }

  def addStackFrame(
      symbolTable: Map[Ident, Type],
      paramList: List[Param] = List()
  ): mutable.ListBuffer[Instruction] = {
    val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer.empty

    val stackFrameToAdd = new StackFrame(symbolTable, paramList)
    stackFrameList += stackFrameToAdd

    instructions.addAll(
      List(
        Push(List(FP, LR)),
        Push(List(R4, R5, R6, R7, R8, R10, IP)),
        Move(FP, SP)
      )
    )
    instructions.addAll(assignStackSpace(stackFrameToAdd.currVarOffset))

    instructions
  }

  def removeStackFrame(
      fun: Boolean = false
  ): mutable.ListBuffer[Instruction] = {
    val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer.empty

    val stackFrameToRemove = stackFrameList.remove(stackFrameList.length - 1)

    instructions.addAll(
      List(
        Pop(List(R4, R5, R6, R7, R8, R10, IP))
        // Push(List(FP))
      )
    )
    instructions.addAll(
      if (!fun) unassignStackSpace(stackFrameToRemove.currVarOffset) else List()
    )
    instructions += Pop(List(FP))

    instructions
  }

  private def assignStackSpace(
      spaceRequired: Int
  ): mutable.ListBuffer[Instruction] = {
    if (spaceRequired == 0)
      mutable.ListBuffer.empty
    else {
      val instructions: mutable.ListBuffer[Instruction] =
        mutable.ListBuffer.empty
      instructions += SubInstr(SP, SP, ImmVal(spaceRequired))
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
      instructions += AddInstr(SP, SP, ImmVal(spaceAcquired))
      instructions
    }
  }

  def getIdentOffset(id: Ident): Int = {
    stackFrameList.reverse.foreach(stackFrame => {
      stackFrame.getVar(id) match {
        case Some(x) => {}
        case None    =>
      }
    })
    0
  }
}

class StackFrame(symbolTable: Map[Ident, Type], paramList: List[Param]) {
  var declaredVarMap: Map[Ident, Int] = Map.empty

  var currVarOffset = 0
  var currArgOffset = 0

  def getVar(id: Ident): Option[Int] = declaredVarMap.get(id)
}

/*
  def offset(name: String) : Int = {
    // size of stack frames previous to the one where name is located
    var prevScopeSizes = 0
    // the offset in bytes
    var result = 0
    // whether the variable is in scope
    var varFound = false

    for (sF <- stackFrames.reverse) {
      if (!varFound) {
        sF.findVar(name) match {
          case Some(x) =>
             varFound = true
            result = x
          case None =>
        }
      }
      if (!varFound) {
        if(sF.isFunction) {
          // goes past all variables and params on stack frame
          // extra 2* WORD_SIZE is for the push(lr) and push(fp) per stack frame
          prevScopeSizes += sF.pushedArgsSize + sF.localVarSize + 2 * WORD_SIZE
        } else {
          // goes past all variables on stack frame
          // extra WORD_SIZE is for the push(fp) per stack frame
          prevScopeSizes += sF.localVarSize + WORD_SIZE

        }
      }
    }
    if (!varFound) {
      throw new Exception(s"Couldn't find variable \'$name\' on stack!")
    }
    prevScopeSizes + result
  }
}
 */
