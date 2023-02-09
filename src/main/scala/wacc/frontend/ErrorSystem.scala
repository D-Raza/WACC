package wacc.frontend
import parsley.errors.ErrorBuilder
import wacc.AST.Ident
import parsley.errors.Token
import parsley.errors.tokenextractors.LexToken

object errors {

    sealed trait WaccErrorLines

    case class WaccError(pos: (Int, Int), lines: WaccErrorLines)

    // case class SyntaxError(unexpected: Option[WaccErrorItem], expecteds: Set[WaccErrorItem], reasons: Seq[String]) extends WaccErrorLines
    case class SyntaxError(unexpected: Option[String], expected: Option[String], reasons: Seq[String]) extends WaccErrorLines

    sealed trait SemanticError extends WaccErrorLines

    sealed trait WaccErrorItem

    // Mapping out the type of semantic errors
    // TODO: Need to create line info object instead of passing in pos? 

    case class UndefinedVariable(ident: Ident, pos: (Int, Int)) extends SemanticError
    case class RedefinedVariable(ident: Ident, pos: (Int, Int)) extends SemanticError
    case class UndefinedFunction(ident: Ident, pos: (Int, Int)) extends SemanticError
    case class RedefinedFunction(ident: Ident, pos: (Int, Int)) extends  SemanticError
    case class NumberOfArgs(ident: Ident, unexpectedArgoNo: Int, expectedArgNo: Int, pos: (Int, Int)) extends SemanticError
    case class NullException(pos: (Int, Int)) extends SemanticError

    case class TypeError() extends SemanticError


    class ErrorSystemBuilder extends ErrorBuilder[WaccError] {

      type ErrorInfoLines = WaccErrorLines
      type Source = Option[String]
      type Position = (Int, Int)
      type LineInfo = Seq[String]
      type Message = String
      type Messages = Seq[Message]
      type ExpectedItems = Option[String]
      type ExpectedLine = Option[String] // Set[WaccErrorItem]
      type UnexpectedLine = Option[String] //Option[WaccErrorItem]
      type Item = String
      type EndOfInput = String
      type Named = String
      type Raw = String
      

      override def format(pos: Position, source: Source, lines: ErrorInfoLines): WaccError = ???
      
      override def pos(line: Int, col: Int): Position = (line, col)

      
      override def source(sourceName: Option[String]): Source = sourceName
      
      override def vanillaError(unexpected: UnexpectedLine, expected: ExpectedLine, reasons: Messages, line: LineInfo): ErrorInfoLines 
        = SyntaxError(unexpected, expected, reasons) 

      override def specialisedError(msgs: Messages, line: LineInfo): ErrorInfoLines 
        = SyntaxError(None, None, msgs)

      override def combineExpectedItems(alts: Set[Item]): ExpectedItems = Option(alts.toList.filter(_.nonEmpty).mkString(", "))

      override def combineMessages(alts: Seq[Message]): Messages = alts.toList

      override def unexpected(item: Option[Item]): UnexpectedLine = item

      override def expected(alts: ExpectedItems): ExpectedLine = alts

      override def reason(reason: String): Message = reason

      override def message(msg: String): Message = msg

      override def lineInfo(line: String, linesBefore: Seq[String], linesAfter: Seq[String], errorPointsAt: Int, errorWidth: Int): LineInfo = ???

      override val numLinesBefore: Int = 1 // 0? 

      override val numLinesAfter: Int = 1 // 0?

      override def raw(item: String): Raw = item 

      override def named(item: String): Named = item 
      
      override val endOfInput: EndOfInput = "end of input"

     // LexToken?
      def unexpectedToken(cs: Iterable[Char], amountOfInputParserWanted: Int, lexicalError: Boolean): Token 
        = ???
    }
}


/*
case class TestError(pos: (Int, Int), lines: TestErrorLines)

sealed trait TestErrorLines
case class VanillaError(unexpected: Option[TestErrorItem], expecteds: Set[TestErrorItem], reasons: Set[String]) extends TestErrorLines
case class SpecialisedError(msgs: Set[String]) extends TestErrorLines

sealed trait TestErrorItem
case class TestRaw(item: String) extends TestErrorItem
case class TestNamed(item: String) extends TestErrorItem
case object TestEndOfInput extends TestErrorItem
*/


