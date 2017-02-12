package punkt0
package lexer

sealed class Token extends Positioned

case object BAD extends Token         // invalid token
case object EOF extends Token
case object COLON extends Token       // :
case object SEMICOLON extends Token   // ;
case object DOT extends Token         // .
case object COMMA extends Token       // ,
case object EQSIGN extends Token      // =
case object EQUALS extends Token      // ==
case object BANG extends Token        // !
case object LPAREN extends Token      // (
case object RPAREN extends Token      // )
case object LBRACE extends Token      // {
case object RBRACE extends Token      // }
case object AND extends Token         // &&
case object OR extends Token          // ||
case object LESSTHAN extends Token    // <
case object PLUS extends Token        // +
case object MINUS extends Token       // -
case object TIMES extends Token       // *
case object DIV extends Token         // /
case object OBJECT extends Token      // object
case object CLASS extends Token       // class
case object DEF extends Token         // def
case object VAR extends Token         // var
case object UNIT extends Token        // Unit
case object STRING extends Token      // String
case object EXTENDS extends Token     // extends
case object INT extends Token         // Int
case object BOOLEAN extends Token     // Boolean
case object WHILE extends Token       // while
case object IF extends Token          // if
case object ELSE extends Token        // else
case object LENGTH extends Token      // length
case object TRUE extends Token        // true
case object FALSE extends Token       // false
case object THIS extends Token        // this
case object NEW extends Token         // new
case object PRINTLN extends Token     // println

// Identifiers
case class ID(value: String) extends Token

// Integer literals
case class INTLIT(value: Int) extends Token

// String literals
case class STRLIT(value: String) extends Token
