package wacc.extension.peephole
import scala.collection.mutable
import wacc.backend._

object Peephole {

  def peephole()(implicit
      instructions: mutable.ListBuffer[Instruction]
  ): mutable.ListBuffer[Instruction] = {
    var instr = instructions
    instr = removeRedundantSubAndAdd()(instr)
    instr = removeRedundantMov()(instr)
    instr = removeRedundantLoad()(instr)  
    instr = replaceMovAddWithAdd()(instr)
    instr = replaceSubLdrWithLdr()(instr)
    instr = removeRedundantStrLdr()(instr)
    instr = replacePushMovPopWithMov()(instr)
    instr
  }

  /* Removes instructions of the form:
   * sub rx, rx, #0
   * add rx, rx, #0
   *
   */
  def removeRedundantSubAndAdd()(implicit
      instructions: mutable.ListBuffer[Instruction]
  ): mutable.ListBuffer[Instruction] = {
    instructions.filter(_ match {
      case SubInstr(dstReg, srcReg, ImmVal(0), Condition.AL, None) => dstReg.n != srcReg.n
      case AddInstr(dstReg, srcReg, ImmVal(0), Condition.AL, None) => dstReg.n != srcReg.n
      case _                                                       => true
    })
  }

  /* Removes instructions of the form:
   * mov rx rx
   *
   */
  def removeRedundantMov()(implicit
      instructions: mutable.ListBuffer[Instruction]
  ): mutable.ListBuffer[Instruction] = {
    instructions.filter(_ match {
      case Move(dstReg, src, Condition.AL) => {
        src match {
          case srcReg: Register => srcReg.n != dstReg.n
          case _                => true
        }
      }
      case _ => true
    })
  }



  /*
   * Replaces instructions of the form:
   * ldr    |a| #x
   * ldr    |a| #y
   *
   * to:
   * ldr    |a| #y
   *
   */
  def removeRedundantLoad()(implicit instructions: mutable.ListBuffer[Instruction]): mutable.ListBuffer[Instruction] = {
    val optimisedInstructions = mutable.ListBuffer.empty[Instruction]
    var i = 0
    while (i < instructions.length) {
      instructions(i) match {
        case Load(dstReg, ImmVal(_), Condition.AL) => {
          if (i + 1 < instructions.length) {
            instructions(i + 1) match {
              case Load(dstReg, ImmVal(immVal), Condition.AL) => {
                optimisedInstructions += Load(dstReg, ImmVal(immVal), Condition.AL)
                i += 1
              }
              case _ => optimisedInstructions += instructions(i)
            }
          }
        }
        case _ => optimisedInstructions += instructions(i)
      }
      i += 1
    }
    optimisedInstructions
  }


  /*
   * Replaces instructions of the form:
   * mov     |a|, #|b|
   * add     |c|, |d|, |e|
   *
   * with:
   * add     |c|, |d|, #|b| when |a| == |e|
   *
   */
  def replaceMovAddWithAdd()(implicit instructions: mutable.ListBuffer[Instruction]): mutable.ListBuffer[Instruction] = {
    val optimisedInstructions = mutable.ListBuffer.empty[Instruction]
    var i = 0
    while (i < instructions.length) {
      instructions(i) match {
        case Move(dstReg, ImmVal(immVal), Condition.AL) => {
          if (i + 1 < instructions.length) {
            instructions(i + 1) match {
              case AddInstr(dstReg2, operand1, operand2, Condition.AL, None) => {
                operand2 match {
                  case reg: Register => {
                    if (dstReg.n == reg.n) {
                        optimisedInstructions += AddInstr(dstReg2, operand1, ImmVal(immVal))
                        i += 1
                    } 
                    else optimisedInstructions += instructions(i)
                  }
                  case _ => optimisedInstructions += instructions(i)
                }
              }
              case _ => optimisedInstructions += instructions(i)
            }
          }
        }
        case _ => optimisedInstructions += instructions(i)
      }
      i += 1
    }
    optimisedInstructions
  }

  /*
   * Replaces instructions of the form:
   * sub     |a|, |b|, #|c|
   * ldr     |d|, [|e|, #0]
   *
   * with:
   * ldr     |d|, [|b|, #-|c|] when |a| == |e|
   *
   */
  def replaceSubLdrWithLdr()(implicit instructions: mutable.ListBuffer[Instruction]): mutable.ListBuffer[Instruction]= {
    val optimisedInstructions = mutable.ListBuffer.empty[Instruction]
    var i = 0
    while (i < instructions.length) {
      instructions(i) match {
        case SubInstr(dstReg, operand2, ImmVal(immVal), Condition.AL, None) => {
          if (i + 1 < instructions.length) {
            instructions(i + 1) match {
              case Load(dstReg2, OffsetMode(_dstReg, None, None, ImmVal(0)), Condition.AL) => {
                operand2 match {
                  case _: Register => {
                    if (dstReg.n == _dstReg.n) {
                      optimisedInstructions += Load(dstReg2, OffsetMode(operand2, shiftAmount = ImmVal(-immVal)))
                      i += 1
                    } 
                    else optimisedInstructions += instructions(i)
                  }
                  case _ => optimisedInstructions += instructions(i)
                }
              }
              case _ => optimisedInstructions += instructions(i)
            }
          }
        }
        case _ => optimisedInstructions += instructions(i)
      }
      i += 1
    }
    optimisedInstructions
  }



  /*
   * Removes redundant instructions in:
   * str     |a|, |b|
   * ldr     |a|, |b| (redundant)
   *
   * or:
   * ldr     |a|, |b|
   * str     |a|, |b| (redundant)
   *
   */

  def removeRedundantStrLdr()(implicit instructions: mutable.ListBuffer[Instruction]): mutable.ListBuffer[Instruction] = {
    val optimisedInstructions = mutable.ListBuffer.empty[Instruction]
    var i = 0
    while (i < instructions.length) {
      instructions(i) match {
        case Store(srcReg, operand2, Condition.AL) => {
          if (i + 1 < instructions.length) {
            instructions(i + 1) match {
              case Load(dstReg, operand2_, Condition.AL) => {
                if (srcReg.n == dstReg.n && operand2 == operand2_) {
                  optimisedInstructions += instructions(i)
                  i += 1
                }
                else optimisedInstructions += instructions(i)
              }
              case _ => optimisedInstructions += instructions(i)
            }
          }
        } 
        case Load(dstReg, operand2, Condition.AL) => {
          if (i + 1 < instructions.length) {
            instructions(i + 1) match {
              case Store(srcReg, operand2_, Condition.AL) => {
                if (srcReg.n == dstReg.n && operand2 == operand2_) {
                  optimisedInstructions += instructions(i)
                  i += 1
                }
                else optimisedInstructions += instructions(i)
              }
              case _ => optimisedInstructions += instructions(i)
            }
          }
        }
        case _ => optimisedInstructions += instructions(i)
      }
      i += 1
    }
    optimisedInstructions
  }

  /*
   * Replaces instructions of the form:
   * push {regs}
   * mov |a|, |b|
   * pop {regs}
   * 
   * with:
   * mov |a|, |b| when |a|, |b| not in regs
   *
   */
  def replacePushMovPopWithMov()(implicit instructions: mutable.ListBuffer[Instruction]): mutable.ListBuffer[Instruction] = {
    val replacedInstructions = mutable.ListBuffer.empty[Instruction]
    var i = 0
    while (i < instructions.length) {
      instructions(i) match {
        case Push(regs, Condition.AL) => {
          if (i + 1 < instructions.length) {
            instructions(i + 1) match {
              case Move(srcReg, operand, Condition.AL) => {
                if (i + 2 < instructions.length) {
                  instructions(i + 2) match {
                    case Pop(regs_, Condition.AL) => {
                      operand match {
                        case _: Register /*| _: ImmChar | _: ImmVal */ => {
                          if (!regs.contains(srcReg) && !regs.contains(operand) && regs.equals(regs_)) {
                            replacedInstructions += Move(srcReg, operand)
                            i += 2
                          } else {
                            replacedInstructions += instructions(i)
                          }
                        }

                        case _: ImmChar | _: ImmVal => {
                          if (!regs.contains(srcReg) && regs.equals(regs_)) {
                            replacedInstructions += Move(srcReg, operand)
                            i += 2
                          } 
                          else replacedInstructions += instructions(i)
                          
                        }

                        case _ => replacedInstructions += instructions(i)
                      }
                    }
                    case _ => replacedInstructions += instructions(i)
                  }
                }
              }
              case _ => replacedInstructions += instructions(i)
              
            }
          }
        }
        case _ => replacedInstructions += instructions(i)
        
      }
      i += 1
    }
    replacedInstructions
  }
   // def replacePushMovPopWithMov()(implicit
  //     instructions: mutable.ListBuffer[Instruction]
  // ): mutable.ListBuffer[Instruction] = {
  //   val patternLength = 3
  //   instructions
  //     .sliding(patternLength)
  //     .flatMap {
  //       case mutable.ListBuffer(Push(regs, Condition.AL), Move(srcReg, operand, Condition.AL), Pop(regs_, Condition.AL)) => {
  //           operand match {
  //             case _: Register | _: ImmChar | _: ImmVal => {
  //               if (!regs.contains(srcReg) && !regs.contains(operand) && regs == regs_) {
  //                 mutable.ListBuffer(Move(srcReg, operand))
  //               } else {
  //                 mutable.ListBuffer(Push(regs), Move(srcReg, operand), Pop(regs))
  //               }
  //             } 
  //           }
  //       }
  //       case x => x 
  //     }
  //     .foldLeft(mutable.ListBuffer.empty[Instruction])((acc, instr) => acc += instr)
  // }

}