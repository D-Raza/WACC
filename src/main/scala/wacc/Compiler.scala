package wacc

import parsley.{Failure, Success}
import wacc.frontend.Parser._
import wacc.frontend.SemanticAnalyser._
import wacc.AST._
import wacc.backend._
import wacc.backend.ARMAssemblyPrinter

import java.io.{File, PrintWriter}
import scala.io.Source
import scala.collection.mutable
import wacc.backend.CodeGenerator
import wacc.backend.CodeGenState

object Compiler {
  private val SYNTAX_ERR_CODE = 100
  private val SEMANTIC_ERR_CODE = 200
  private val FILE_ERR_CODE = 150

  var DEBUG = false
  var PARALLEL = false
  var OPTIMIZE = false

  def main(args: Array[String]): Unit = {
    // Checking for an input file
    if (args.length < 1) {
      System.err.println("  Too few arguments!")
      System.err.println(
        "  Usage: ./compile [FILE] {(P)arallel (D)ebug (O)ptimise}"
      )
      System.exit(FILE_ERR_CODE)
    }

    val inputFile = new File(args(0))

    val restArgs = args.drop(1)
    DEBUG = restArgs.contains("D")
    PARALLEL = restArgs.contains("P")
    OPTIMIZE = restArgs.contains("O")

    val (program, exitCode) =
      parseFile(inputFile, mutable.Set(inputFile.getCanonicalPath()))

    exitCode match {
      case 0 => {
        println("Assembling...")
        implicit val instructions = mutable.ListBuffer[Instruction]()
        implicit val state = CodeGenState()

        val startTime = System.nanoTime()
        instructions ++= CodeGenerator.compileProgram(
          program.get,
          PARALLEL,
          OPTIMIZE
        )
        val elapsedTime = System.nanoTime() - startTime

        if (DEBUG) {
          println("Instructions:")
          instructions.foreach(println)
          println()
          println(
            s"${if (PARALLEL) "Parallel"
              else "Sequential"} code generation${if (OPTIMIZE) " with optimisation "
              else " "}took ${elapsedTime / 1000000}ms"
          )
          println()
        }

        val outputAsm = ARMAssemblyPrinter.printAsm(instructions.toList)

        val printWriter = new PrintWriter(
          inputFile.getName.split('.').head + ".s"
        )

        printWriter.write(outputAsm)
        printWriter.write("\n")
        printWriter.close()
      }
      case _ => ()
    }

    println("Exit code: " + exitCode)
    System.exit(exitCode)
  }

  def parseFile(
      inputFile: File,
      alreadyImported: mutable.Set[String]
  ): (Option[Program], Int) = {
    // File validation
    if (!inputFile.exists()) {
      System.err.println(f"$inputFile does not exist! Exiting...")
      System.exit(FILE_ERR_CODE)
    }
    if (!inputFile.isFile()) {
      System.err.println(f"$inputFile is not a file! Exiting...")
      System.exit(FILE_ERR_CODE)
    }

    if (DEBUG) {
      println("Source file:")
      Source.fromFile(inputFile).getLines().foreach(println);
      println()
    }

    // Frontend
    println(s"Compiling $inputFile...")

    val parseResult = parse(inputFile)

    parseResult match {
      case Success(x) => {
        if (DEBUG) {
          print("Parse tree: ")
          println(x)
          println()
        }

        implicit val source: File = inputFile
        var program = x.program

        if (DEBUG && !x.imports.isEmpty) {
          println("Imports:")
          x.imports.foreach(println)
          println()
        }

        var importFiles = x.imports
          .map(inputFile.getParent + "/" + _.filepath)
          .map(new File(_))
          .filter(fp => !alreadyImported.contains(fp.getCanonicalPath()))
          .distinctBy(_.getCanonicalPath())
        while (!importFiles.isEmpty) {
          val f = importFiles.head
          parseFile(f, alreadyImported += f.getCanonicalPath()) match {
            case (Some(importProgram), _) => {
              program = program.copy(funcs =
                program.funcs ++ importProgram.funcs
              )(NULLPOS)
            }
            case (None, exitCode) => {
              println(s"Error(s) parsing imported file: ${f.getName}!")
              return (None, exitCode)
            }
          }
          importFiles = importFiles.tail
        }

        val errors = checkProgramSemantics(program)
        if (errors.isEmpty) {
          println(s"No errors found in $inputFile!")
          println()
          (Some(program), 0)
        } else {
          println(s"Error(s) found in $inputFile:")
          errors.foreach(println)
          println()
          (None, SEMANTIC_ERR_CODE)
        }
      }
      case Failure(msg) => { println(msg); (None, SYNTAX_ERR_CODE) }
    }
  }

}
