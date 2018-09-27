package punkt0.lexer

import scala.util.matching.Regex

sealed abstract class TokenMatcher(tokenKind: TokenKind, literalString: String = null, matchRegex: String = null) {
  var regex: Regex =
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

case object COLON_MATCHER extends TokenMatcher(COLON,":")

case object SEMICOLON_MATCHER extends TokenMatcher(SEMICOLON,";")

case object DOT_MATCHER extends TokenMatcher(DOT,".")

case object COMMA_MATCHER extends TokenMatcher(COMMA,",")

case object EQSIGN_MATCHER extends TokenMatcher(EQSIGN,"=")

case object EQUALS_MATCHER extends TokenMatcher(EQUALS,"==")

case object BANG_MATCHER extends TokenMatcher(BANG, "!")

case object LPAREN_MATCHER extends TokenMatcher(LPAREN, "(")

case object RPAREN_MATCHER extends TokenMatcher(RPAREN, ")")

case object LBRACE_MATCHER extends TokenMatcher(LBRACE,"{")

case object RBRACE_MATCHER extends TokenMatcher(RBRACE,"}")

case object AND_MATCHER extends TokenMatcher(AND,"&&")

case object OR_MATCHER extends TokenMatcher(OR,"||")

case object LESSTHAN_MATCHER extends TokenMatcher(LESSTHAN,"<")

case object PLUS_MATCHER extends TokenMatcher(PLUS, "+")

case object MINUS_MATCHER extends TokenMatcher(MINUS, "-")

case object TIMES_MATCHER extends TokenMatcher(TIMES, "*")

case object DIV_MATCHER extends TokenMatcher(DIV, "/")

case object OBJECT_MATCHER extends TokenMatcher(OBJECT, "object")

case object CLASS_MATCHER extends TokenMatcher(CLASS, "class")

case object DEF_MATCHER extends TokenMatcher(DEF, "def")

case object OVERRIDE_MATCHER extends TokenMatcher(OVERRIDE, "override")

case object VAR_MATCHER extends TokenMatcher(VAR, "var")

case object UNIT_MATCHER extends TokenMatcher(UNIT, "Unit")

case object STRING_MATCHER extends TokenMatcher(STRING, "String")

case object EXTENDS_MATCHER extends TokenMatcher(EXTENDS, "extends")

case object INT_MATCHER extends TokenMatcher(INT, "Int")

case object BOOLEAN_MATCHER extends TokenMatcher(BOOLEAN, "Boolean")

case object WHILE_MATCHER extends TokenMatcher(WHILE,"while")

case object IF_MATCHER extends TokenMatcher(IF,"if")

case object ELSE_MATCHER extends TokenMatcher(ELSE, "else")

case object TRUE_MATCHER extends TokenMatcher(TRUE, "true")

case object FALSE_MATCHER extends TokenMatcher(FALSE, "false")

case object THIS_MATCHER extends TokenMatcher(THIS, "this")

case object NULL_MATCHER extends TokenMatcher(NULL, "null")

case object NEW_MATCHER extends TokenMatcher(NEW, "new")

case object PRINTLN_MATCHER extends TokenMatcher(PRINTLN, "println")


object TOKEN_MATCHERS {

  // Order here is important, earlier means that it has a higher priority
  val asList: List[TokenMatcher] = List(
    COLON_MATCHER,
    SEMICOLON_MATCHER,
    DOT_MATCHER,
    COMMA_MATCHER,
    EQSIGN_MATCHER,
    EQUALS_MATCHER,
    BANG_MATCHER,
    LPAREN_MATCHER,
    RPAREN_MATCHER,
    LBRACE_MATCHER,
    RBRACE_MATCHER,
    AND_MATCHER,
    OR_MATCHER,
    LESSTHAN_MATCHER,
    PLUS_MATCHER,
    MINUS_MATCHER,
    TIMES_MATCHER,
    DIV_MATCHER,
    OBJECT_MATCHER,
    CLASS_MATCHER,
    DEF_MATCHER,
    OVERRIDE_MATCHER,
    VAR_MATCHER,
    UNIT_MATCHER,
    STRING_MATCHER,
    EXTENDS_MATCHER,
    INT_MATCHER,
    BOOLEAN_MATCHER,
    WHILE_MATCHER,
    IF_MATCHER,
    ELSE_MATCHER,
    TRUE_MATCHER,
    FALSE_MATCHER,
    THIS_MATCHER,
    NULL_MATCHER,
    NEW_MATCHER,
    PRINTLN_MATCHER,
  )
}