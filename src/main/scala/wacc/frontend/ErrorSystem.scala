package wacc.frontend
import parsley.errors.combinator._
import wacc.AST.Ident

object errors {

    sealed trait WaccErrorLines

    case class WaccError(pos: (Int, Int), lines: WaccErrorLines)

    case class SyntaxError(unexpected: Option[WaccErrorItem], expecteds: Set[WaccErrorItem], reasons: Set[String]) extends WaccErrorLines

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


