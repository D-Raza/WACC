package wacc

import parsley.{Failure, Success}
import wacc.frontend.Parser._
import wacc.frontend.SemanticAnalyser._
import wacc.backend._
import wacc.backend.ARMAssemblyPrinter

import java.io.{File, PrintWriter}
import scala.io.Source
import scala.collection.mutable
import wacc.backend.CodeGenerator
import wacc.backend.CodeGeneratorState

object Compiler {
  val DEBUG = true

  val DUMMY_ASM = """|.data
                     |.text
                     |.global main
                     |main:
                     |    push {fp, lr}
                     |    push {r8, r10, r12}
                     |    mov fp, sp
                     |    mov r0, #0
                     |    pop {r8, r10, r12}
                     |    pop {fp, pc}
                """.stripMargin

  def main(args: Array[String]): Unit = {
    // Argument checking
    val expectedNoArgs = 1
    if (args.length != expectedNoArgs) {
      System.err.print(args.length compare expectedNoArgs match {
        case -1 => "Too few arguments!"
        case 1  => "Too many arguments!"
      })
      System.err.println(" Exiting...")
      System.err.println("  Usage: ./compile [FILE]")
      System.exit(-1)
    }

    // File validation
    val inputFile = new File(args(0))
    if (!inputFile.exists()) {
      System.err.println(f"$inputFile does not exist! Exiting...")
      System.exit(-1)
    }
    if (!inputFile.isFile()) {
      System.err.println(f"$inputFile is not a file! Exiting...")
      System.exit(-1)
    }

    if (DEBUG)
      Source.fromFile(inputFile).getLines().foreach(println);

    // Frontend
    println("Compiling...")

    val parseResult = parse(inputFile)
    var exitCode = 0;

    parseResult match {
      case Success(x) => {
        if (DEBUG) {
          print("Parse tree: ")
          println(x)
          println()
        }

        implicit val source: File = inputFile
        val errors = checkProgramSemantics(x)
        if (errors.isEmpty) {
          println("No errors found!")

          println("Assembling...")
          implicit val instructions = mutable.ListBuffer[Instruction]()
          CodeGenerator.compileProgram(x, CodeGeneratorState())

          if (DEBUG) {
            println("Instructions:")
            instructions.foreach(println)
            println()
          }

          val outputAsm = ARMAssemblyPrinter.printAsm(instructions.toList)

          val printWriter = new PrintWriter(
            inputFile.getName.split('.').head + ".s"
          )
          
          printWriter.write(outputAsm)
          printWriter.write("\n")
          printWriter.close()
        } else {
          println("Errors found:")
          errors.foreach(println)
          exitCode = 200
        }
      }
      case Failure(msg) => { println(msg); exitCode = 100 }
    }

    println("Exit code: " + exitCode)
    System.exit(exitCode)
  }
}
