package wacc.frontend

import wacc.frontend.Lexer._
import wacc.AST._
import java.io.File
import scala.io.Source
import parsley.Parsley
import parsley.Parsley._
import parsley.errors.ErrorBuilder
import parsley.errors.tokenextractors.LexToken

object errors {

  case class WACCLineInfo(
      line: String,
      linesBefore: Seq[String],
      linesAfter: Seq[String],
      errorPointsAt: Int,
      errorWidth: Int
  ) {
    private val errorLineStart = " |"
    private def errorPointer(caretAt: Int, errorWidth: Int) =
      s"${" " * caretAt}${"^" * errorWidth}"

    def genErrorInfo: String = {
      (linesBefore.map(line => s"$errorLineStart$line") ++:
        Seq(
          s"$errorLineStart$line",
          s"$errorLineStart${errorPointer(errorPointsAt, errorWidth)}"
        ) ++:
        linesAfter.map(line => s"$errorLineStart$line")).mkString("\n")
    }
  }

  object WACCLineInfo {
    def genLineInfo(pos: (Int, Int))(implicit source: File): WACCLineInfo = {
      val (line, col) = pos
      val sourceLines = Source.fromFile(source).getLines().toArray
      val lineBefore = if (line > 1) sourceLines(line - 2) else ""
      val lineAfter = if (line < sourceLines.length) sourceLines(line) else ""
      val errorWidth = 1
      WACCLineInfo(
        sourceLines(line - 1),
        Seq(lineBefore),
        Seq(lineAfter),
        col,
        errorWidth
      )
    }
  }

  sealed trait WACCErrorInfoLines {
    val errorLines: Seq[String]
    val lineInfo: WACCLineInfo
  }

  case class WACCError(
      pos: (Int, Int),
      source: File,
      lines: WACCErrorInfoLines
  ) {
    override def toString: String = {
      val errorType = lines match {
        case SyntaxError(_, _, _, _) => "Syntax error"
        case _: SemanticError        => "Semantic error"
      }
      val (line, col) = pos

      val genErrorInfoWithLineNumbers = lines.lineInfo.genErrorInfo
        .split("\n")
        .zipWithIndex
        .map { case (l, index) => s"${index + line - 1}$l" }
        .mkString("\n")

      s"""${errorType} in ${source.getName()} at line ${line}, col ${col}:
          >${lines.errorLines.mkString("\n")}
          >${genErrorInfoWithLineNumbers}
          >
        """.stripMargin('>')
    }
  }

  case class SyntaxError(
      unexpected: Option[String],
      expected: Option[String],
      reasons: Seq[String],
      lineInfo: WACCLineInfo
  ) extends WACCErrorInfoLines {
    override val errorLines: Seq[String] = {
      val unexpectedLine = unexpected match {
        case Some(unexpected) => s"Unexpected: $unexpected"
        case None             => ""
      }
      val expectedLine = expected match {
        case Some(expected) => s"Expected: $expected"
        case None           => ""
      }
      val reasonsLine = reasons match {
        case Nil     => ""
        case reasons => s"Reasons: ${reasons.mkString(", ")}"
      }
      Seq(unexpectedLine, expectedLine, reasonsLine).filter(_.nonEmpty)
    }
  }

  sealed trait SemanticError extends WACCErrorInfoLines

  case class UndefinedVariableError(ident: Ident, lineInfo: WACCLineInfo)
      extends SemanticError {
    override val errorLines: Seq[String] = Seq(
      s"Variable ${ident.name} undefined"
    )
  }
  case class RedefinedVariableError(ident: Ident, lineInfo: WACCLineInfo)
      extends SemanticError {
    override val errorLines: Seq[String] = Seq(
      s"Variable ${ident.name} already defined"
    )
  }
  case class UndefinedFunctionError(ident: Ident, lineInfo: WACCLineInfo)
      extends SemanticError {
    override val errorLines: Seq[String] = Seq(
      s"Function ${ident.name} undefined"
    )
  }
  case class RedefinedFunctionError(ident: Ident, lineInfo: WACCLineInfo)
      extends SemanticError {
    override val errorLines: Seq[String] = Seq(
      s"Function ${ident.name} already defined"
    )
  }
  case class IncorrectNumberOfArgsError(
      ident: Ident,
      gotNoArgs: Int,
      expectedNoArgs: Int,
      lineInfo: WACCLineInfo
  ) extends SemanticError {
    override val errorLines: Seq[String] = Seq(
      s"Incorrect number of arguments for function ${ident.name}. Expected ${expectedNoArgs} arguments, got ${gotNoArgs} arguments"
    )
  }

  case class TypeMismatchError(
      gotType: Type,
      expectedType: Set[Type],
      additionalPosInfo: String,
      lineInfo: WACCLineInfo
  ) extends SemanticError {
    def containsErrorType(ty: Type): Boolean = ty match {
      case ErrorType()   => true
      case ArrayType(ty) => containsErrorType(ty)
      case PairType(fstType, sndType) =>
        containsErrorType(fstType.asType) || containsErrorType(sndType.asType)
      case _ => false
    }

    override val errorLines: Seq[String] = Seq(
      s"Type mismatch error${additionalPosInfo match {
          case "" => ""
          case _  => " at " + additionalPosInfo
        }}:${if (containsErrorType(gotType)) ""
        else " got " + gotType.toString + ","} expected ${expectedType.size match {
          case 1 => expectedType.head
          case _ => "one of " + expectedType.mkString(", ")
        }}"
    )
  }

  case class UnexpectedReturnError(stat: Stat, lineInfo: WACCLineInfo)
      extends SemanticError {
    override val errorLines: Seq[String] = Seq(
      "Return outside function is not allowed"
    )
  }

  case class ArrayDimensionMismatchError(
      gotDims: Int,
      expectedDims: Int,
      lineInfo: WACCLineInfo
  ) extends SemanticError {
    override val errorLines: Seq[String] = Seq(
      s"Array dimension mismatch: got ${gotDims}, expected ${expectedDims}"
    )
  }

  object UndefinedVariableError {
    def genError(id: Ident)(implicit source: File): WACCError = {
      WACCError(
        id.pos,
        source,
        new UndefinedVariableError(id, WACCLineInfo.genLineInfo(id.pos))
      )
    }
  }

  object RedefinedVariableError {
    def genError(id: Ident)(implicit source: File): WACCError = {
      WACCError(
        id.pos,
        source,
        new RedefinedVariableError(id, WACCLineInfo.genLineInfo(id.pos))
      )
    }
  }

  object UndefinedFunctionError {
    def genError(id: Ident)(implicit source: File): WACCError = {
      WACCError(
        id.pos,
        source,
        new UndefinedFunctionError(id, WACCLineInfo.genLineInfo(id.pos))
      )
    }
  }

  object RedefinedFunctionError {
    def genError(id: Ident)(implicit source: File): WACCError = {
      WACCError(
        id.pos,
        source,
        new RedefinedFunctionError(id, WACCLineInfo.genLineInfo(id.pos))
      )
    }
  }

  object TypeMismatchError {
    def genError(
        gotType: Type,
        expectedType: Set[Type],
        pos: (Int, Int),
        additionalPosInfo: String
    )(implicit source: File): WACCError = {
      WACCError(
        pos,
        source,
        new TypeMismatchError(
          gotType,
          expectedType,
          additionalPosInfo,
          WACCLineInfo.genLineInfo(pos)
        )
      )
    }
  }

  object UnexpectedReturnError {
    def genError(stat: Stat)(implicit source: File): WACCError = {
      WACCError(
        stat.pos,
        source,
        new UnexpectedReturnError(stat, WACCLineInfo.genLineInfo(stat.pos))
      )
    }
  }

  object ArrayDimensionMismatchError {
    def genError(gotDims: Int, expectedDims: Int, pos: (Int, Int))(implicit
        source: File
    ): WACCError = {
      WACCError(
        pos,
        source,
        new ArrayDimensionMismatchError(
          gotDims,
          expectedDims,
          WACCLineInfo.genLineInfo(pos)
        )
      )
    }
  }

  object IncorrectNumberOfArgsError {
    def genError(ident: Ident, gotNoArgs: Int, expectedNoArgs: Int)(implicit
        source: File
    ) = {
      WACCError(
        ident.pos,
        source,
        new IncorrectNumberOfArgsError(
          ident,
          gotNoArgs,
          expectedNoArgs,
          WACCLineInfo.genLineInfo(ident.pos)
        )
      )
    }
  }

  class WACCErrorBuilder extends ErrorBuilder[WACCError] with LexToken {

    val tokenParsers = Lexer.keywords.map { keyword =>
      lookAhead(attempt(parsley.character.string(keyword)))
    }

    override def tokens: Seq[Parsley[String]] = Seq(
      lexer.nonlexeme.names.identifier
    ) ++ tokenParsers.toSeq

    type ErrorInfoLines = WACCErrorInfoLines
    type Source = File
    type Position = (Int, Int)
    type LineInfo = WACCLineInfo
    type Message = String
    type Messages = Seq[Message]
    type ExpectedItems = Option[String]
    type ExpectedLine = Option[String]
    type UnexpectedLine = Option[String]
    type Item = String
    type EndOfInput = String
    type Named = String
    type Raw = String

    override def format(
        pos: Position,
        source: Source,
        lines: ErrorInfoLines
    ): WACCError = WACCError(pos, source, lines)

    override def pos(line: Int, col: Int): Position = (line, col)

    override def source(source: Option[String]): Source = new File(source.get)

    override def vanillaError(
        unexpected: UnexpectedLine,
        expected: ExpectedLine,
        reasons: Messages,
        line: LineInfo
    ): ErrorInfoLines = SyntaxError(unexpected, expected, reasons, line)

    override def specialisedError(
        msgs: Messages,
        line: LineInfo
    ): ErrorInfoLines = SyntaxError(None, None, msgs, line)

    override def combineExpectedItems(alts: Set[Item]): ExpectedItems = Some(
      alts.mkString(", ").replaceAll(", ([^,]+)$", " or $1")
    )

    override def combineMessages(alts: Seq[Message]): Messages = alts.toList

    override def unexpected(item: Option[Item]): UnexpectedLine = item

    override def expected(alts: ExpectedItems): ExpectedLine = alts

    override def reason(reason: String): Message = reason

    override def message(msg: String): Message = msg

    override def lineInfo(
        line: String,
        linesBefore: Seq[String],
        linesAfter: Seq[String],
        errorPointsAt: Int,
        errorWidth: Int
    ): LineInfo =
      WACCLineInfo(line, linesBefore, linesAfter, errorPointsAt, errorWidth)

    override val numLinesBefore: Int = 1

    override val numLinesAfter: Int = 1

    override def raw(item: String): Raw = item

    override def named(item: String): Named = item

    override val endOfInput: EndOfInput = "end of file"
  }

}
