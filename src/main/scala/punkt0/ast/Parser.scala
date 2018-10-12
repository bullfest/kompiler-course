package punkt0
package ast

import Trees._
import lexer._

object Parser extends Phase[Iterator[Token], Program] {
  def run(tokens: Iterator[Token])(ctx: Context): Program = {
    import Reporter._
    /** Store the current token, as read from the lexer. */
    var currentToken: Token = new Token(BAD)

    def readToken(): Unit = {
      if (tokens.hasNext) {
        // uses nextToken from the Lexer trait
        currentToken = tokens.next

        // skips bad tokens
        while (currentToken.kind == BAD) {
          currentToken = tokens.next
        }
      }
    }

    /** ''Eats'' the expected token, or terminates with an error. */
    def eat(kind: TokenKind): Unit = {
      if (currentToken.kind == kind) {
        readToken()
      } else {
        expected(kind)
      }
    }

    /** Complains that what was found was not expected. The method accepts arbitrarily many arguments of type TokenKind */
    def expected(kind: TokenKind, more: TokenKind*): Nothing = {
      fatal("expected: " + (kind::more.toList).mkString(" or ") + ", found: " + currentToken, currentToken)
    }

    def parseGoal: Program = {
      parseProgram
    }

    def parseProgram: Program = {
      val thisToken = currentToken
      var classes: List[ClassDecl] = List()
      while (currentToken.kind == CLASS) {
        classes ::= parseClass
      }
      val main = parseMain
      eat(EOF)
      Program(main, classes.reverse).setPos(thisToken)
    }

    def parseClass: ClassDecl = {
      eat(CLASS)
      val name = parseIdentifier()
      var extends_ : Option[Identifier] = None
      if (currentToken.kind == EXTENDS) {
        eat(EXTENDS)
        extends_ = Some(parseIdentifier())
      }
      eat(LBRACE)
      var vars: List[VarDecl] = List()
      while (currentToken.kind == VAR) {
        vars ::= parseVar
      }
      var methods: List[MethodDecl] = List()
      while (currentToken.kind == OVERRIDE || currentToken.kind == DEF) {
        methods ::= parseMethod
      }
      eat(RBRACE)

      ClassDecl(name, extends_, vars.reverse, methods.reverse)
    }

    def parseMain: MainDecl = {
      eat(OBJECT)
      val name = parseIdentifier()
      eat(EXTENDS)
      val extends_ = parseIdentifier()
      eat(LBRACE)
      var vars: List[VarDecl] = List()
      while (currentToken.kind == VAR) {
        vars ::= parseVar
      }
      var expressions: List[ExprTree] = List(parseExpression)
      while (currentToken.kind == SEMICOLON) {
        eat(SEMICOLON)
        expressions ::= parseExpression
      }
      eat(RBRACE)
      MainDecl(name, extends_, vars.reverse, expressions.reverse)
    }

    def parseMethod: MethodDecl = {
      var overrides = false
      if (currentToken.kind == OVERRIDE) {
        overrides = true
        eat(OVERRIDE)
      }
      eat(DEF)
      val name = parseIdentifier()
      eat(LPAREN)
      var args: List[Formal] = List()
      if (currentToken.kind == IDKIND) {
        args ::= parseFormal
        while (currentToken.kind == COMMA) {
          eat(COMMA)
          args ::= parseFormal
        }
      }
      eatTokenSequence(List(RPAREN, COLON))
      val retType = parseType
      eatTokenSequence(List(EQSIGN, LBRACE))
      var vars: List[VarDecl] = List()
      while (currentToken.kind == VAR)
        vars ::= parseVar

      var expressions: List[ExprTree] = List(parseExpression)
      while (currentToken.kind == SEMICOLON) {
        eat(SEMICOLON)
        expressions ::= parseExpression
      }
      eat(RBRACE)

      MethodDecl(overrides, retType, name, args.reverse, vars.reverse, expressions.tail.reverse, expressions.head)
    }

    def parseFormal: Formal = {
      val id = parseIdentifier()
      eat(COLON)
      val type_ = parseType
      Formal(type_, id)
    }

    def parseVar: VarDecl = {
      eat(VAR)
      val id = parseIdentifier()
      eat(COLON)
      val type_ = parseType
      eat(EQSIGN)
      val expr = parseExpression
      eat(SEMICOLON)
      VarDecl(type_, id, expr)
    }

    def parseType: TypeTree = {
      val thisToken = currentToken
      (thisToken.kind match {
        case INT =>
          eat(INT)
          IntType()
        case STRING =>
          eat(STRING)
          StringType()
        case UNIT =>
          eat(UNIT)
          UnitType()
        case BOOLEAN =>
          eat(BOOLEAN)
          BooleanType()
        case IDKIND =>
          eat(IDKIND)
          parseIdentifier(thisToken)
        case _ =>
          expected(INT, STRING, UNIT, BOOLEAN, IDKIND)
      }).setPos(thisToken)
    }

    def parseWeakExpression: ExprTree = {
      val thisToken = currentToken

      var tree: ExprTree = currentToken.kind match {
        case INTLITKIND =>
          eat(INTLITKIND)
          IntLit(thisToken.asInstanceOf[INTLIT].value)
        case STRLITKIND =>
          eat(STRLITKIND)
          StringLit(thisToken.asInstanceOf[STRLIT].value)
        case TRUE =>
          eat(TRUE)
          True()
        case FALSE =>
          eat(FALSE)
          False()
        case IDKIND =>
          val identifier = parseIdentifier()
          if (currentToken.kind == EQSIGN) {
            // ident == expr
            eat(EQSIGN)
            Assign(identifier, parseExpression)
          } else // ident
            identifier
        case THIS =>
          eat(THIS)
          This()
        case NULL =>
          eat(NULL)
          Null()
        case NEW =>
          eat(NEW)
          val identifier = parseIdentifier()
          eatTokenSequence(List(LPAREN, RPAREN))
          New(identifier)
        case BANG =>
          eat(BANG)
          Not(parseWeakExpression)
        case LPAREN =>
          eat(LPAREN)
          val exprTree = parseExpression
          eat(RPAREN)
          exprTree
        case LBRACE =>
          eat(LBRACE)
          var exprList: List[ExprTree] = List()
          if (currentToken.kind != RBRACE) {
            exprList ::= parseExpression
            while (currentToken.kind != RBRACE) {
              eat(SEMICOLON)
              exprList ::= parseExpression
            }
          }
          eat(RBRACE)
          Block(exprList.reverse)
        case IF =>
          eat(IF)
          eat(LPAREN)
          val condExpr = parseExpression
          eat(RPAREN)
          val trueExpr = parseExpression
          if (currentToken.kind == ELSE) {
            eat(ELSE)
            If(condExpr, trueExpr, Some(parseExpression))
          } else {
            If(condExpr, trueExpr, None)
          }
        case WHILE =>
          eat(WHILE)
          eat(LPAREN)
          val condExpr = parseExpression
          eat(RPAREN)
          While(condExpr, parseExpression)
        case PRINTLN =>
          eat(PRINTLN)
          eat(LPAREN)
          val expr = parseExpression
          eat(RPAREN)
          Println(expr)
        case _ =>
          expected(
            INTLITKIND, STRLITKIND, TRUE, FALSE, IDKIND, THIS,
            NULL, NEW, BANG, LPAREN, LBRACE, IF, WHILE, PRINTLN
          )
      }

      while (currentToken.kind == DOT) {
        eat(DOT)
        val method = parseIdentifier()
        eat(LPAREN)
        var args: List[ExprTree] = List()
        if (currentToken.kind != RPAREN) {
          args = parseExpression :: args
          while (currentToken.kind == COMMA) {
            eat(COMMA)
            args = parseExpression :: args
          }
        }
        eat(RPAREN)
        tree = MethodCall(tree, method, args.reverse)
      }

      tree.setPos(thisToken)
    }

    def eatTokenSequence(tokenKindList: List[TokenKind]): Unit =
      tokenKindList.foreach(eat)

    def parseIdentifier(token: Token = null): Identifier = {
      var token_ = token
      if (token == null) {
        token_ = currentToken
        eat(IDKIND)
      }
      Identifier(token_.asInstanceOf[ID].value)
    }

    def parseStrongExpression4: ExprTree = {
      var expr = parseWeakExpression
      while (List(TIMES, DIV).contains(currentToken.kind)) {
        currentToken.kind match {
          case TIMES =>
            eat(TIMES)
            expr = Times(expr, parseWeakExpression)
          case DIV =>
            eat(DIV)
            expr = Div(expr, parseWeakExpression)
          case _ =>
            expr
        }
      }
      expr
    }

    def parseStrongExpression3: ExprTree = {
      var expr = parseStrongExpression4
      while (List(PLUS, MINUS).contains(currentToken.kind)) {
        currentToken.kind match {
          case PLUS =>
            eat(PLUS)
            expr = Plus(expr, parseStrongExpression4)
          case MINUS =>
            eat(MINUS)
            expr = Minus(expr, parseStrongExpression4)
          case _ =>
            expr
        }
      }
      expr
    }

    def parseStrongExpression2: ExprTree = {
      var expr = parseStrongExpression3
      while (List(LESSTHAN, EQUALS).contains(currentToken.kind)) {
        currentToken.kind match {
          case LESSTHAN =>
            eat(LESSTHAN)
            expr = LessThan(expr, parseStrongExpression3)
          case EQUALS =>
            eat(EQUALS)
            expr = Equals(expr, parseStrongExpression3)
          case _ =>
            expr
        }
      }
      expr
    }

    def parseStrongExpression1: ExprTree = {
      var expr = parseStrongExpression2
      while (currentToken.kind == AND) {
        eat(AND)
        expr = And(expr, parseStrongExpression2)
      }
      expr
    }

    def parseExpression: ExprTree = {
      var expr = parseStrongExpression1
      while (currentToken.kind == OR) {
        eat(OR)
        expr = Or(expr, parseStrongExpression1)
      }
      expr
    }

    readToken()
    val tree = parseGoal
    terminateIfErrors()
    tree
  }
}
