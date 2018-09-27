package punkt0.lexer

import scala.util.matching.Regex

sealed abstract class TokenMatcher(tokenKind: TokenKind, literalString: String = null, matchRegex: String = null) {
  var regex:Regex =
    if (matchRegex == null)
      null
    else
      new Regex(matchRegex)

  def getTokenKind: TokenKind = tokenKind

  def token = new Token(tokenKind)

  def matchingPrefixLength(s: String): Int =
    if (literalString != null)
      commonPrefixLength(s, literalString)
    else if (matchRegex != null)
      regex.findPrefixMatchOf(s) match {
        case Some(prefix) => prefix.matched.length
        case None => 0
      }
    else
      throw new IllegalStateException("TokenMatcher has to have a literalString or matchRegex")


  private def commonPrefixLength(s: String, t: String): Int = {
    var i = 0
    while (i < math.min(s.length, t.length) && s.charAt(i) == t.charAt(i))
      i += 1
    i
  }

  def matches(s: String): Boolean = {
    if (literalString != null)
      literalString == s
    else if (matchRegex != null)
      s.matches(matchRegex)
    else
      throw new IllegalStateException("TokenMatcher has to have a literalString or matchRegex")
  }
}

case object COLON_MATCHER extends TokenMatcher(COLON, literalString = ":")
case object SEMICOLON_MATCHER extends TokenMatcher(SEMICOLON, literalString = ";")
case object WHILE_MATCHER extends TokenMatcher(WHILE, literalString = "while")
case object IF_MATCHER extends TokenMatcher(IF, literalString = "if")


object TOKEN_MATCHERS {

  // Order here is important, earlier means that it has a higher priority
  val asList: List[TokenMatcher] = List(
    COLON_MATCHER,
    SEMICOLON_MATCHER,
    WHILE_MATCHER,
    IF_MATCHER
  )
}