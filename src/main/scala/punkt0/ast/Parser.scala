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
      var exprs: List[ExprTree] = List(parseExpression)
      while (currentToken.kind == SEMICOLON) {
        eat(SEMICOLON)
        exprs ::= parseExpression
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

      var exprs: List[ExprTree] = List(parseExpression)
      while (currentToken.kind == SEMICOLON) {
        eat(SEMICOLON)
        exprs ::= parseExpression
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
      var expr = parseExpression
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
            Assign(identifier, parseExpression)
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
          Not(parseExpression)
        case LPAREN =>
          val exprTree = parseExpression
          eat(RPAREN)
          exprTree
        case LBRACE =>
          var allIsWell = true
          var exprList: List[ExprTree] = List(parseExpression)
          while (currentToken.kind != RPAREN)
            exprList ::= parseExpression
          eat(RPAREN)
          Block(exprList.reverse)
        case IF =>
          eat(LPAREN)
          val condExpr = parseExpression
          eat(RPAREN)
          val trueExpr = parseExpression
          if (currentToken.kind == ELSE)
            If(condExpr, trueExpr, Some(parseExpression))
          else
            If(condExpr, trueExpr, None)
        case WHILE =>
          eat(LPAREN)
          val condExpr = parseExpression
          eat(RPAREN)
          While(condExpr, parseExpression)
        case PRINTLN =>
          eat(LPAREN)
          val expr = parseExpression
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
            args = parseExpression :: args
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

    def parseIdentifier(token:Token = null): Identifier = {
      var token_ = token
      if (token == null) {
        token_ = currentToken
        eat(IDKIND)
      }
      Identifier(token_.asInstanceOf[ID].value)
    }

    readToken
    val tree = parseGoal
    terminateIfErrors()
    tree
  }
}
