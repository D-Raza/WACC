package wacc

import wacc.frontend.Parser._
import java.io.File
import scala.io.Source
import parsley.{Success, Failure}

object Compiler {
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

    // Frontend
    println("Compiling...")

    val parseResult = parse(inputFile)
    var exitCode = 0;

    parseResult match {
      case Success(x) => {
        for (line <- Source.fromFile(inputFile).getLines()) println(line);
        println(x)
      }
      case Failure(msg) => { println(msg); exitCode = 100 }
    }

    // TODO: Backend
    // println("Assembling...")

    println("Exit code: " + exitCode)
    System.exit(exitCode)
  }
}
