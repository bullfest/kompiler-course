package punkt0
package ast

import Trees._
import lexer._

object Parser extends Phase[Iterator[Token], Program] {
  def run(tokens: Iterator[Token])(ctx: Context): Program = {
    import Reporter._
    /** Store the current token, as read from the lexer. */
    var currentToken: Token = new Token(BAD)

    def readToken: Unit = {
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
        readToken
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
      Program(parseMain, classes.reverse).setPos(thisToken)
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

      ClassDecl(name, extends_, vars, methods)
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
      var exprs: List[ExprTree] = List(parseExpressionWithOperators0)
      while (currentToken.kind == SEMICOLON) {
        eat(SEMICOLON)
        exprs ::= parseExpressionWithOperators0
      }
      eat(RBRACE)
      MainDecl(name, extends_, vars.reverse, exprs.reverse)
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
        while (currentToken.kind == COLON) {
          eat(COLON)
          args ::= parseFormal
        }
      }
      eatTokenSequence(List(RPAREN, COLON))
      val retType = parseType
      eatTokenSequence(List(EQSIGN, LBRACE))
      var vars: List[VarDecl] = List()
      while (currentToken.kind == VAR)
        vars ::= parseVar

        var exprs: List[ExprTree] = List(parseExpressionWithOperators0)
        while (currentToken.kind == SEMICOLON) {
          eat(SEMICOLON)
          exprs ::= parseExpressionWithOperators0
        }
        eat(RBRACE)

        MethodDecl(overrides, retType, name, args.reverse, vars.reverse, exprs.tail.reverse, exprs.head)
    }

    def parseFormal: Formal = {
      val id = parseIdentifier()
      eat(COLON)
      val type_ = parseType
      Formal(type_, id)
    }

    def parseVar: VarDecl = {
      eat(VAR)
      var id = parseIdentifier()
      eat(COLON)
      var type_ = parseType
      eat(EQSIGN)
      var expr = parseExpressionWithOperators0
      eat(SEMICOLON)
      VarDecl(type_, id, expr)
    }

    def parseType: TypeTree = {
      val thisToken = currentToken
      readToken
      (thisToken.kind match {
        case INT =>
          IntType()
        case STRING =>
          StringType()
        case UNIT =>
          UnitType()
        case BOOLEAN =>
          BooleanType()
        case IDKIND =>
          parseIdentifier(thisToken)
      }).setPos(thisToken)
    }

    def parseExpression: ExprTree = {
      val thisToken = currentToken
      readToken

      var tree: ExprTree = thisToken.kind match {
        case INTLITKIND =>
          IntLit(thisToken.asInstanceOf[INTLIT].value)
        case STRLITKIND =>
          StringLit(thisToken.asInstanceOf[STRLIT].value)
        case TRUE =>
          True()
        case FALSE =>
          False()
        case IDKIND =>
          if (currentToken.kind == EQSIGN) {
            // ident == expr
            val identifier = parseIdentifier()
            eat(EQSIGN)
            Assign(identifier, parseExpressionWithOperators0)
          } else // ident
            parseIdentifier(thisToken)
        case THIS =>
          This()
        case NULL =>
          Null()
        case NEW =>
          val identifier = parseIdentifier()
          eatTokenSequence(List(LPAREN, RPAREN))
          New(identifier)
        case BANG =>
          Not(parseExpressionWithOperators0)
        case LPAREN =>
          val exprTree = parseExpressionWithOperators0
          eat(RPAREN)
          exprTree
        case LBRACE =>
          var allIsWell = true
          var exprList: List[ExprTree] = List(parseExpressionWithOperators0)
          while (currentToken.kind != RPAREN)
            exprList ::= parseExpressionWithOperators0
            eat(RPAREN)
            Block(exprList.reverse)
        case IF =>
          eat(LPAREN)
          val condExpr = parseExpressionWithOperators0
          eat(RPAREN)
          val trueExpr = parseExpressionWithOperators0
          if (currentToken.kind == ELSE)
            If(condExpr, trueExpr, Some(parseExpressionWithOperators0))
          else
            If(condExpr, trueExpr, None)
        case WHILE =>
          eat(LPAREN)
          val condExpr = parseExpressionWithOperators0
          eat(RPAREN)
          While(condExpr, parseExpressionWithOperators0)
        case PRINTLN =>
          eat(LPAREN)
          val expr = parseExpressionWithOperators0
          eat(RPAREN)
          Println(expr)
        case _ =>
          expected(INTLITKIND)
      }

      if (currentToken.kind == DOT) {
        eat(DOT)
        val method = parseIdentifier()
        eat(LPAREN)
        var args: List[ExprTree] = List()
        if (currentToken.kind != RPAREN) {
          do {
            args = parseExpressionWithOperators0 :: args
          } while (currentToken.kind == COMMA)
        }
        eat(RPAREN)
        tree = MethodCall(tree, method, args)
      } else if (List(TIMES, DIV, MINUS, PLUS).contains(currentToken.kind)) {
        //TODO: :'(
      }
      tree.setPos(thisToken)
    }

    def eatTokenSequence(tokenKindList: List[TokenKind]): Unit =
      tokenKindList.map(eat)

    def parseIdentifier(token: Token = null): Identifier = {
      var token_ = token
      if (token == null) {
        token_ = currentToken
        eat(IDKIND)
      }
      Identifier(token_.asInstanceOf[ID].value)
    }

    def parseExpressionWithOperators0(): ExprTree = {
      val expr = parseExpressionWithOperators1
      val nextToken = tokens.next
      if (nextToken.kind == TIMES) {
        eat(TIMES)
        Times(expr, parseExpressionWithOperators1)
      } else if (nextToken.kind == DIV) {
        eat(DIV)
        Div(expr, parseExpressionWithOperators1)
      } else {
        expr
      }
    }

    def parseExpressionWithOperators1(): ExprTree = {
      val expr = parseExpression
      val nextToken = tokens.next
      if (nextToken.kind == PLUS) {
        eat(PLUS)
        Plus(expr, parseExpression)
      } else if (nextToken.kind == MINUS) {
        eat(MINUS)
        Minus(expr, parseExpression)
      } else {
        expr
      }
    }

    readToken
    val tree = parseGoal
    terminateIfErrors()
    tree
  }
}
