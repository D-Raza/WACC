package wacc.extension

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import parsley.{Failure, Success}
import wacc.frontend.Parser._
import wacc.frontend.SemanticAnalyser._
import java.io.File
import sys.process._
import java.io.PrintWriter
import wacc.backend.CodeGenerator
import wacc.backend._
import wacc.backend.CodeGenState

import scala.collection.mutable

class OverloadingTests extends AnyFlatSpec {

  // gets a list of all files in a directory and its subdirectories
  private def getFilesInDir(dir: String): List[String] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).map(_.getAbsolutePath).toList :::
        d.listFiles
          .filter(_.isDirectory)
          .flatMap(f => getFilesInDir(f.getAbsolutePath))
          .toList
    } else
      Nil
  }

  // Returns the exit code (just frontend)
  private def getExitCode(dir: String): Int = {
    val files = getFilesInDir(dir)
    var exitCode = 0
    for (file <- files) {
      val inputFile = new File(file)
      val parseResult = parse(inputFile)
      parseResult match {
        case Success(x) => {
          implicit val source: File = inputFile
          val errors = checkProgramSemantics(x)
          if (!errors.isEmpty)
            exitCode = 200
        }
        case Failure(_) => exitCode = 100
      }
    }
    exitCode
  }


  // Compile and emulate 
  private def compileFiles(dir: String) = {
    val files = getFilesInDir(dir)
    for (file <- files) {
      val inputFile = new File(file)

      // Run the parser
      val parseResult = parse(inputFile)
      parseResult should matchPattern {
        case Success(_) =>
      }

      // Get the AST
      val ast = parseResult.get

      // Check semantics 
      val errors = checkProgramSemantics(ast)(inputFile)
      errors should be (Nil) 

      // Run the backend
      implicit val state = CodeGenState()
      implicit val instructions = mutable.ListBuffer[Instruction]()
      instructions ++= CodeGenerator.compileProgram(ast)(state)

      // Write the .s file
      val noExtension = file.split("\\.").head
      val asmPath = s"$noExtension.s"
      val asmFile = new File(asmPath)
      val printWriter = new PrintWriter(asmFile)
      val outputASM = ARMAssemblyPrinter.printAsm(instructions.toList)
      printWriter.write(outputASM)
      printWriter.write("\n")
      printWriter.close()

      // Check that the .s file compiles and the output is the same as the expected output 
      val gccCommand = s"arm-linux-gnueabi-gcc -o ${noExtension} -mcpu=arm1176jzf-s -mtune=arm1176jzf-s $asmPath"
      gccCommand.! should be (0)

      // Remove the .s and the executable
      asmFile.delete()
      new File(noExtension).delete()
    }
  }

  "All valid programs with overloading" should "return exit code 0" in {
    val exitCode = getExitCode("wacc_overloading_examples/valid")
    assert(exitCode == 0)
  }

  "All invalid programs with overloading" should "return exit code 200" in {
    val exitCode = getExitCode("wacc_overloading_examples/invalid")
    assert(exitCode == 200)
  }

  "All valid programs with overloading" should "compile" in {
    compileFiles("wacc_overloading_examples/valid")
  }  
}
