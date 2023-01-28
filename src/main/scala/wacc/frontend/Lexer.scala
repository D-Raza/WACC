package wacc.frontend

import parsley.Parsley
import parsley.token.Lexer
import parsley.token.descriptions.LexicalDesc

object Lexer {
  private val desc = LexicalDesc.plain.copy(
    spaceDesc = LexicalDesc.plain.spaceDesc.copy(
      commentLine = "#"
    )
  )
  private val lexer = new Lexer(desc)

  def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}
