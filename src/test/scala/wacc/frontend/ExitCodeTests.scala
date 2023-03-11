// package wacc.frontend

// import org.scalatest.flatspec.AnyFlatSpec
// import parsley.{Failure, Success}
// import wacc.frontend.Parser._
// import wacc.frontend.SemanticAnalyser._

// import java.io.File
// import sys.process._

// class ExitCode extends AnyFlatSpec {

//   // gets a list of all files in a directory and its subdirectories
//   private def getFilesInDir(dir: String): List[String] = {
//     val d = new File(dir)
//     if (d.exists && d.isDirectory) {
//       d.listFiles.filter(_.isFile).map(_.getAbsolutePath).toList :::
//         d.listFiles
//           .filter(_.isDirectory)
//           .flatMap(f => getFilesInDir(f.getAbsolutePath))
//           .toList
//     } else
//       Nil
//   }

//   private def compileFilesInDir(dir: String): Int = {
//     val files = getFilesInDir(dir)
//     var exitCode = 0
//     for (file <- files) {
//       val inputFile = new File(file)
//       val parseResult = parse(inputFile)
//       parseResult match {
//         case Success(x) => {
//           implicit val source: File = inputFile
//           val errors = checkProgramSemantics(x)
//           if (!errors.isEmpty)
//             exitCode = 200
//         }
//         case Failure(_) => exitCode = 100
//       }
//     }
//     exitCode
//   }

//   // clone repo
//   "git clone https://gitlab.doc.ic.ac.uk/lab2223_spring/wacc_examples.git/ --quiet".!

//   "All valid programs" should "return exit code 0" in {
//     val exitCode = compileFilesInDir("wacc_examples/valid")
//     assert(exitCode == 0)
//   }

//   "All syntactically invalid programs" should "return exit code 100" in {
//     val exitCode = compileFilesInDir("wacc_examples/invalid/syntaxErr")
//     assert(exitCode == 100)
//   }

//   "All semantically invalid programs" should "return exit code 200" in {
//     val exitCode = compileFilesInDir("wacc_examples/invalid/semanticErr")
//     assert(exitCode == 200)
//   }

// }
