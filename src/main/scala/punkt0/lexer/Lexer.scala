package punkt0
package lexer

import java.io.File
import scala.collection.mutable.ListBuffer


object Lexer extends Phase[File, Iterator[Token]] {
  import Reporter._

  def run(f: File)(ctx: Context): Iterator[Token] = {
    val source = scala.io.Source.fromFile(f)
    val tokens = new ListBuffer[Token]
    var program = source.mkString
    var tokenMatcher: TokenMatcher = null
    var currentPosition = 0
    source.close()

    while (tokenMatcher.getTokenKind != EOF) {
      tokenMatcher = TOKEN_MATCHERS.asList.maxBy(_.matchingPrefixLength(program))
      val subStringLength = tokenMatcher.matchingPrefixLength(program) // Should be O(|prefix|)
      val subString = program.substring(0, subStringLength)
      program = program.substring(subStringLength) //substring is O(1) as String is immutable

      if (tokenMatcher.matches(subString)) {
        val token = tokenMatcher.token
        token.setPos(f, currentPosition)
        tokens += token
      } else {
        val token = new Token(BAD)
        token.setPos(f, currentPosition)
        tokens += token
      }

      currentPosition += subStringLength
    }
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
