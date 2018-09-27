package punkt0
package lexer

import java.io.File
import scala.collection.mutable.ListBuffer


object Lexer extends Phase[File, Iterator[Token]] {
  import Reporter._


  def run(f: File)(ctx: Context): Iterator[Token] = {
    val source = scala.io.Source.fromFile(f)
    val tokens = new ListBuffer[Token]

    // TODO: implement this method

    var tokenList = tokens.toList
    new Iterator[Token] {

      def hasNext: Boolean = {
        tokenList.nonEmpty
      }

      def next: Token = {
        val token = tokenList.head
        tokenList = tokenList.tail
        token
      }
    }
  }
}
