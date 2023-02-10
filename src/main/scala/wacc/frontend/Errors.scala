package wacc.frontend

import parsley.Parsley
import parsley.Parsley._
import parsley.errors.ErrorBuilder
import parsley.errors.tokenextractors.LexToken
import wacc.AST._
import wacc.frontend.Lexer._

import java.io.File
import scala.io.Source

object Errors {

  private val numLinesBeforeError = 1
  private val numLinesAfterError = 1

  sealed trait WACCErrorInfoLines {
    val errorLines: Seq[String]
    val lineInfo: WACCLineInfo
  }

  sealed trait SemanticError extends WACCErrorInfoLines

  case class WACCLineInfo(
      line: String,
      linesBefore: Seq[String],
      linesAfter: Seq[String],
      errorCol: Int,
      errorWidth: Int
  ) {
    private val infoLineStart = ">"
    def genErrorInfo: String = {
      (linesBefore.map(line => s"$infoLineStart$line") ++:
        Seq(
          s"$infoLineStart$line",
          s"$infoLineStart${errorPointer(errorCol, errorWidth)}"
        ) ++:
        linesAfter.map(line => s"$infoLineStart$line")).mkString("\n")
    }

    private def errorPointer(errorCol: Int, errorWidth: Int) = {
      val pointerSpace = " " * (errorCol)
      if (errorWidth == 0)
        pointerSpace + "^"
      else
        pointerSpace + ("^" * errorWidth)
    }
  }

  case class WACCError(
      pos: (Int, Int),
      source: File,
      lines: WACCErrorInfoLines
  ) {
    override def toString: String = {
      val (line, col) = pos
      val errorType = lines match {
        case SyntaxError(_, _, _, _) => "Syntax error"
        case _: SemanticError        => "Semantic error"
      }

      val lineInfo = errorType match {
        case "Semantic error" =>
          lines.lineInfo.copy(errorCol = lines.lineInfo.errorCol - 1)
        case _ => lines.lineInfo
      }

      s"""$errorType in ${source.getName} at line $line, col $col:
          |${lines.errorLines.mkString("\n")}
          |${lineInfo.genErrorInfo}
          |
        """.stripMargin
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
      s"Incorrect number of arguments for function ${ident.name}. Expected $expectedNoArgs arguments, got $gotNoArgs arguments"
    )
  }

  case class TypeMismatchError(
      gotType: Type,
      expectedTypes: Set[Type],
      additionalPosInfo: String,
      lineInfo: WACCLineInfo
  ) extends SemanticError {
    override val errorLines: Seq[String] =
      if (
        expectedTypes.exists(
          ErrorType()(NULLPOS) equiv _
        ) && (gotType equiv UnknownType()(NULLPOS))
      )
        Seq(
          "Types of both sides of the assignment must be known - pairs are unknown here due to type erasure"
        )
      else
        Seq(
          s"Type mismatch error${additionalPosInfo match {
              case "" => ""
              case _  => " at " + additionalPosInfo
            }}:${if (containsErrorType(gotType)) ""
            else " got " + gotType.toString + ","} expected ${expectedTypes.size match {
              case 1 => expectedTypes.head
              case _ => "one of " + expectedTypes.mkString(", ")
            }}"
        )

    private def containsErrorType(ty: Type): Boolean = ty match {
      case ErrorType()   => true
      case ArrayType(ty) => containsErrorType(ty)
      case PairType(fstType, sndType) =>
        containsErrorType(fstType.asType) || containsErrorType(sndType.asType)
      case _ => false
    }
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
      s"Array dimension mismatch: got $gotDims, expected $expectedDims"
    )
  }

  class WACCErrorBuilder extends ErrorBuilder[WACCError] with LexToken {

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
    override val numLinesBefore: Int = numLinesBeforeError
    override val numLinesAfter: Int = numLinesAfterError
    override val endOfInput: EndOfInput = "end of file"
    private val tokenParsers = Lexer.keywords.map { keyword =>
      lookAhead(attempt(parsley.character.string(keyword)))
    }

    override def tokens: Seq[Parsley[String]] = Seq(
      lexer.nonlexeme.names.identifier
    ) ++ tokenParsers.toSeq

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
    ): ErrorInfoLines = {
      SyntaxError(unexpected, expected, reasons, line)
    }

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

    override def raw(item: String): Raw = item

    override def named(item: String): Named = item
  }

  object WACCLineInfo {
    def genLineInfo(pos: (Int, Int), idLength: Int)(implicit
        source: File
    ): WACCLineInfo = {
      val (line, col) = pos
      val sourceLines = Source.fromFile(source).getLines().toArray
      val lineBefore = line match {
        case x if x > 1 => sourceLines(line - numLinesBeforeError - 1)
        case _          => ""
      }
      val lineAfter = line match {
        case x if x < sourceLines.length =>
          sourceLines(line + numLinesAfterError - 1)
        case _ => ""
      }
      WACCLineInfo(
        sourceLines(line - 1),
        Seq(lineBefore),
        Seq(lineAfter),
        col,
        idLength
      )
    }
  }

  object UndefinedVariableError {
    def genError(id: Ident)(implicit source: File): WACCError = {
      val pos = id.pos
      val errorWidth = id.name.length()
      WACCError(
        pos,
        source,
        new UndefinedVariableError(
          id,
          WACCLineInfo.genLineInfo(id.pos, errorWidth)
        )
      )
    }
  }

  object RedefinedVariableError {
    def genError(id: Ident)(implicit source: File): WACCError = {
      val pos = id.pos
      val errorWidth = id.name.length()
      WACCError(
        pos,
        source,
        new RedefinedVariableError(
          id,
          WACCLineInfo.genLineInfo(pos, errorWidth)
        )
      )
    }
  }

  object UndefinedFunctionError {
    def genError(id: Ident)(implicit source: File): WACCError = {
      val pos = id.pos
      val errorWidth = id.name.length()
      WACCError(
        pos,
        source,
        new UndefinedFunctionError(
          id,
          WACCLineInfo.genLineInfo(pos, errorWidth)
        )
      )
    }
  }

  object RedefinedFunctionError {
    def genError(id: Ident)(implicit source: File): WACCError = {
      val pos = id.pos
      val errorWidth = id.name.length()
      WACCError(
        pos,
        source,
        new RedefinedFunctionError(
          id,
          WACCLineInfo.genLineInfo(pos, errorWidth)
        )
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
          WACCLineInfo.genLineInfo(pos, 1)
        )
      )
    }
  }

  object UnexpectedReturnError {
    def genError(stat: Stat)(implicit source: File): WACCError = {
      val pos = stat.pos
      val errorWidth = "return".length()
      WACCError(
        pos,
        source,
        new UnexpectedReturnError(
          stat,
          WACCLineInfo.genLineInfo(pos, errorWidth)
        ) // return.length
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
          WACCLineInfo.genLineInfo(pos, 1)
        )
      )
    }
  }

  object IncorrectNumberOfArgsError {
    def genError(ident: Ident, gotNoArgs: Int, expectedNoArgs: Int)(implicit
        source: File
    ): WACCError = {
      val pos = ident.pos
      WACCError(
        pos,
        source,
        new IncorrectNumberOfArgsError(
          ident,
          gotNoArgs,
          expectedNoArgs,
          WACCLineInfo.genLineInfo(pos, 1)
        )
      )
    }
  }

}
