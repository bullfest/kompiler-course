package punkt0
package lexer

import java.io.File
import scala.collection.mutable.ListBuffer


object Lexer extends Phase[File, Iterator[Token]] {
  import Reporter._

  def run(f: File)(ctx: Context): Iterator[Token] = {
    var source = scala.io.Source.fromFile(f)
    val tokens = new ListBuffer[Token]
    var program = source.mkString
    var tokenMatcher: TokenMatcher = null

    source = scala.io.Source.fromFile(f)
    source.next()

    while (program != "") {
      tokenMatcher = TOKEN_MATCHERS.asList.maxBy(_.matchingPrefixLength(program))
      val subStringLength = tokenMatcher.matchingPrefixLength(program) // Should be O(|prefix|)
      val subString = program.substring(0, subStringLength)

      if (tokenMatcher.getTokenKind != COMMENT && tokenMatcher.getTokenKind != WHITESPACE) {
        if (tokenMatcher.matches(subString)) {
          val token = tokenMatcher.token(subString)
          token.setPos(f, source.pos)
          tokens += token
        } else {
          val token = new Token(BAD)
          token.setPos(f, source.pos)
          tokens += token
          Reporter.error("Unrecognized token", token)
        }
      }

      program = program.substring(subStringLength) //substring is O(1) as String is immutable

      (0 until subStringLength).foreach(_ => if (source.hasNext) source.next())

      if (subStringLength == 0) {
        // If we don't match anything, proceed
        program = program.substring(1)
        if (source.hasNext) source.next()
      }
    }

    tokens += new Token(EOF)
    source.close()

    var tokenList = tokens.toList
    new Iterator[Token] {

      def hasNext: Boolean = tokenList.nonEmpty

      def next: Token = {
        val token = tokenList.head
        tokenList = tokenList.tail
        token
      }
    }
  }
}
