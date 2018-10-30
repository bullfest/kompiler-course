package punkt0
package ast

import analyzer.Symbols._

object Trees {
  val indent_length = 2

  sealed trait Tree extends Positioned {
    def prettyPrint(sb: StringBuilder, indent: Int, doSymbolIds: Boolean): Unit
  }

  case class Program(main: MainDecl, classes: List[ClassDecl]) extends Tree {
    override def prettyPrint(sb: StringBuilder, indent: Int, doSymbolIds: Boolean): Unit = {
      classes.foreach(c => {
        c.prettyPrint(sb, indent, doSymbolIds); sb.append("\n\n")
      })
      main.prettyPrint(sb, indent, doSymbolIds)
      sb.append("\n")
    }
  }

  case class MainDecl(obj: Identifier, parent: Identifier, vars: List[VarDecl], exprs: List[ExprTree]) extends Tree with Symbolic[ClassSymbol] {

    def collectSymbol: ClassSymbol = {
      val symbol = new ClassSymbol(obj.value)
      setSymbol(symbol)
      obj.setSymbol(symbol)
      for (variable <- vars) {
        val varSymbol = variable.collectSymbol
        symbol.members += (varSymbol.name -> varSymbol)
      }
      symbol
    }

    override def prettyPrint(sb: StringBuilder, indent: Int, doSymbolIds: Boolean): Unit = {
      sb.append(" " * indent).append("object ")
      obj.prettyPrint(sb, indent, doSymbolIds)
      sb.append(" extends ")
      parent.prettyPrint(sb, indent, doSymbolIds)
      sb.append(" {\n")
      val indent2 = indent + 2
      for (var_ <- vars) {
        sb.append(" " * indent2)
        var_.prettyPrint(sb, indent2, doSymbolIds)
        sb.append("\n")
      }
      if (exprs.nonEmpty) {
        sb.append(" " * indent2)
        exprs.head.prettyPrint(sb, indent2, doSymbolIds)
        for (expression <- exprs.tail) {
          sb.append(";\n")
          sb.append(" " * indent2)
          expression.prettyPrint(sb, indent2, doSymbolIds)
        }
      }
      sb.append("\n")
      sb.append(" " * indent).append("}")
    }
  }

  case class ClassDecl(id: Identifier, parent: Option[Identifier], vars: List[VarDecl], methods: List[MethodDecl]) extends Tree with Symbolic[ClassSymbol] {

    def collectSymbol: ClassSymbol = {
      val symbol = new ClassSymbol(id.value)
      setSymbol(symbol)
      id.setSymbol(symbol)
      for (variable <- vars) {
        val varSymbol = variable.collectSymbol
        symbol.members += (varSymbol.name -> varSymbol)
      }
      for (method <- methods) {
        val methodSymbol = method.collectSymbol(symbol)
        symbol.methods += (methodSymbol.name -> methodSymbol)
      }
      symbol
    }

    override def prettyPrint(sb: StringBuilder, indent: Int, doSymbolIds: Boolean): Unit = {
      sb.append(" " * indent).append("class ")
      id.prettyPrint(sb, indent, doSymbolIds)
      parent match {
        case Some(p) =>
          sb.append(" extends ")
          p.prettyPrint(sb, indent, doSymbolIds)
        case None => Unit
      }
      sb.append(" {\n")
      val indent2 = indent + indent_length
      for (var_ <- vars) {
        sb.append(" " * indent2)
        var_.prettyPrint(sb, indent2, doSymbolIds)
        sb.append("\n")
      }
      for (meth <- methods) {
        sb.append(" " * indent2)
        meth.prettyPrint(sb, indent2, doSymbolIds)
        sb.append("\n\n")
      }
      sb.append(" " * indent).append("}")
    }
  }

  case class VarDecl(tpe: TypeTree, id: Identifier, expr: ExprTree) extends Tree with Symbolic[VariableSymbol] {

    def collectSymbol: VariableSymbol = {
      val symbol = new VariableSymbol(id.value)
      id.setSymbol(symbol)
      setSymbol(symbol)
      symbol
    }

    override def prettyPrint(sb: StringBuilder, indent: Int, doSymbolIds: Boolean): Unit = {
      sb.append("var ")
      id.prettyPrint(sb, indent, doSymbolIds)
      sb.append(": ")
      tpe.prettyPrint(sb, indent, doSymbolIds)
      sb.append(" = ")
      expr.prettyPrint(sb, indent, doSymbolIds)
      sb.append(";")
    }
  }

  case class MethodDecl(overrides: Boolean, retType: TypeTree, id: Identifier,
                        args: List[Formal], vars: List[VarDecl], exprs: List[ExprTree],
                        retExpr: ExprTree) extends Tree with Symbolic[MethodSymbol] {

    def collectSymbol(cls: ClassSymbol): MethodSymbol = {
      val symbol = new MethodSymbol(id.value, cls)
      id.setSymbol(symbol)
      setSymbol(symbol)
      for (arg <- args) {
        val argSymbol = arg.collectSymbol
        symbol.params += (argSymbol.name -> argSymbol)
      }
      for (var_ <- vars) {
        val varSymbol = var_.collectSymbol
        symbol.members += (varSymbol.name -> varSymbol)
      }
      symbol
    }

    override def prettyPrint(sb: StringBuilder, indent: Int, doSymbolIds: Boolean): Unit = {
      if (overrides)
        sb.append("override ")
      sb.append("def ")
      id.prettyPrint(sb, indent, doSymbolIds)
      sb.append("(")
      if (args.nonEmpty) {
        args.head.prettyPrint(sb, indent, doSymbolIds)
        for (arg <- args.tail) {
          sb.append(", ")
          arg.prettyPrint(sb, indent, doSymbolIds)
        }
      }
      sb.append("): ")
      retType.prettyPrint(sb, indent, doSymbolIds)
      sb.append(" = {\n")
      val indent2 = indent + indent_length
      for (var_ <- vars) {
        sb.append(" " * indent2)
        var_.prettyPrint(sb, indent2, doSymbolIds)
        sb.append("\n")
      }
      for (expression <- exprs) {
        sb.append(" " * indent2)
        expression.prettyPrint(sb, indent2, doSymbolIds)
        sb.append(";\n")
      }
      sb.dropRight(2).append("\n")
      sb.append(" " * indent2)
      retExpr.prettyPrint(sb, indent2, doSymbolIds)
      sb.append("\n")
      sb.append(" " * indent).append("}")
    }
  }

  sealed case class Formal(tpe: TypeTree, id: Identifier) extends Tree with Symbolic[VariableSymbol] {

    def collectSymbol: VariableSymbol = {
      val symbol = new VariableSymbol(id.value)
      id.setSymbol(symbol)
      setSymbol(symbol)
      symbol
    }

    override def prettyPrint(sb: StringBuilder, indent: Int, doSymbolIds: Boolean): Unit = {
      id.prettyPrint(sb, indent, doSymbolIds)
      sb.append(": ")
      tpe.prettyPrint(sb, indent, doSymbolIds)
    }
  }

  sealed trait TypeTree extends Tree

  case class BooleanType() extends TypeTree {
    override def prettyPrint(sb: StringBuilder, indent: Int, doSymbolIds: Boolean): Unit = {
      sb.append("Boolean")
    }
  }

  case class IntType() extends TypeTree {
    override def prettyPrint(sb: StringBuilder, indent: Int, doSymbolIds: Boolean): Unit = {
      sb.append("Int")
    }
  }

  case class StringType() extends TypeTree {
    override def prettyPrint(sb: StringBuilder, indent: Int, doSymbolIds: Boolean): Unit = {
      sb.append("String")
    }
  }

  case class UnitType() extends TypeTree {
    override def prettyPrint(sb: StringBuilder, indent: Int, doSymbolIds: Boolean): Unit =
      sb.append("Unit")
  }


  sealed trait ExprTree extends Tree

  case class And(lhs: ExprTree, rhs: ExprTree) extends ExprTree {
    override def prettyPrint(sb: StringBuilder, indent: Int, doSymbolIds: Boolean): Unit = {
      lhs.prettyPrint(sb, indent, doSymbolIds)
      sb.append(" && ")
      rhs.prettyPrint(sb, indent, doSymbolIds)
    }
  }

  case class Or(lhs: ExprTree, rhs: ExprTree) extends ExprTree {
    override def prettyPrint(sb: StringBuilder, indent: Int, doSymbolIds: Boolean): Unit = {
      lhs.prettyPrint(sb, indent, doSymbolIds)
      sb.append(" || ")
      rhs.prettyPrint(sb, indent, doSymbolIds)
    }
  }

  case class Plus(lhs: ExprTree, rhs: ExprTree) extends ExprTree {
    override def prettyPrint(sb: StringBuilder, indent: Int, doSymbolIds: Boolean): Unit = {
      lhs.prettyPrint(sb, indent, doSymbolIds)
      sb.append(" + ")
      rhs.prettyPrint(sb, indent, doSymbolIds)
    }
  }

  case class Minus(lhs: ExprTree, rhs: ExprTree) extends ExprTree {
    override def prettyPrint(sb: StringBuilder, indent: Int, doSymbolIds: Boolean): Unit = {
      lhs.prettyPrint(sb, indent, doSymbolIds)
      sb.append(" - ")
      rhs.prettyPrint(sb, indent, doSymbolIds)
    }
  }

  case class Times(lhs: ExprTree, rhs: ExprTree) extends ExprTree {
    override def prettyPrint(sb: StringBuilder, indent: Int, doSymbolIds: Boolean): Unit = {
      lhs.prettyPrint(sb, indent, doSymbolIds)
      sb.append(" * ")
      rhs.prettyPrint(sb, indent, doSymbolIds)
    }
  }

  case class Div(lhs: ExprTree, rhs: ExprTree) extends ExprTree {
    override def prettyPrint(sb: StringBuilder, indent: Int, doSymbolIds: Boolean): Unit = {
      lhs.prettyPrint(sb, indent, doSymbolIds)
      sb.append(" / ")
      rhs.prettyPrint(sb, indent, doSymbolIds)
    }
  }

  case class LessThan(lhs: ExprTree, rhs: ExprTree) extends ExprTree {
    override def prettyPrint(sb: StringBuilder, indent: Int, doSymbolIds: Boolean): Unit = {
      lhs.prettyPrint(sb, indent, doSymbolIds)
      sb.append(" < ")
      rhs.prettyPrint(sb, indent, doSymbolIds)
    }
  }

  case class Equals(lhs: ExprTree, rhs: ExprTree) extends ExprTree {
    override def prettyPrint(sb: StringBuilder, indent: Int, doSymbolIds: Boolean): Unit = {
      lhs.prettyPrint(sb, indent, doSymbolIds)
      sb.append(" == ")
      rhs.prettyPrint(sb, indent, doSymbolIds)
    }
  }

  case class MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree]) extends ExprTree {
    override def prettyPrint(sb: StringBuilder, indent: Int, doSymbolIds: Boolean): Unit = {
      obj.prettyPrint(sb, indent, doSymbolIds)
      sb.append(".")
      meth.prettyPrint(sb, indent, doSymbolIds)
      sb.append("(")
      if (args.nonEmpty) {
        args.head.prettyPrint(sb, indent, doSymbolIds)
        for (arg <- args.tail) {
          sb.append(", ")
          arg.prettyPrint(sb, indent, doSymbolIds)
        }
      }
      sb.append(")")
    }
  }

  case class IntLit(value: Int) extends ExprTree {
    override def prettyPrint(sb: StringBuilder, indent: Int, doSymbolIds: Boolean): Unit =
      sb.append(value)
  }

  case class StringLit(value: String) extends ExprTree {
    override def prettyPrint(sb: StringBuilder, indent: Int, doSymbolIds: Boolean): Unit =
      sb.append("\"").append(value).append("\"")
  }

  case class True() extends ExprTree {
    override def prettyPrint(sb: StringBuilder, indent: Int, doSymbolIds: Boolean): Unit =
      sb.append("true")
  }

  case class False() extends ExprTree {
    override def prettyPrint(sb: StringBuilder, indent: Int, doSymbolIds: Boolean): Unit =
      sb.append("false")
  }

  case class Identifier(value: String) extends TypeTree with ExprTree with Symbolic[Symbol] {
    override def prettyPrint(sb: StringBuilder, indent: Int, doSymbolIds: Boolean): Unit = {
      sb.append(value)
      if (doSymbolIds) {
        sb.append('#').append(if (hasSymbol) getSymbol.id else "??")
      }
    }
  }

  case class This() extends ExprTree with Symbolic[ClassSymbol] {
    override def prettyPrint(sb: StringBuilder, indent: Int, doSymbolIds: Boolean): Unit =
      sb.append("this")
  }

  case class Null() extends ExprTree {
    override def prettyPrint(sb: StringBuilder, indent: Int, doSymbolIds: Boolean): Unit =
      sb.append("null")
  }

  case class New(tpe: Identifier) extends ExprTree {
    override def prettyPrint(sb: StringBuilder, indent: Int, doSymbolIds: Boolean): Unit = {
      sb.append("new ")
      tpe.prettyPrint(sb, indent, doSymbolIds)
      sb.append("()")
    }
  }

  case class Not(expr: ExprTree) extends ExprTree {
    override def prettyPrint(sb: StringBuilder, indent: Int, doSymbolIds: Boolean): Unit = {
      sb.append("!")
      expr.prettyPrint(sb, indent, doSymbolIds)
    }
  }

  case class Block(exprs: List[ExprTree]) extends ExprTree {
    override def prettyPrint(sb: StringBuilder, indent: Int, doSymbolIds: Boolean): Unit = {
      if (exprs.isEmpty)
        sb.append("{}")
      else {
        sb.append("{\n")
        val indent2 = indent + indent_length
        sb.append(" " * indent2)
        exprs.head.prettyPrint(sb, indent2, doSymbolIds)
        exprs.tail.foreach(expr => {
          sb.append("; \n")
          sb.append(" " * indent2)
          expr.prettyPrint(sb, indent2, doSymbolIds)
        })
        sb.append("\n")
        sb.append(" " * indent).append("}")
      }
    }
  }

  case class If(expr: ExprTree, thn: ExprTree, els: Option[ExprTree]) extends ExprTree {
    override def prettyPrint(sb: StringBuilder, indent: Int, doSymbolIds: Boolean): Unit = {
      sb.append("if (")
      expr.prettyPrint(sb, indent, doSymbolIds)
      sb.append(") ")
      val indent2 = indent + indent_length
      if (thn.isInstanceOf[Block]) {
        thn.prettyPrint(sb, indent2, doSymbolIds)
      } else {
        sb.append("\n")
        sb.append(" " * indent2)
        thn.prettyPrint(sb, indent2, doSymbolIds)
      }
      els match {
        case Some(e) =>
          sb.append(" else ")
          if (thn.isInstanceOf[Block]) {
            e.prettyPrint(sb, indent2, doSymbolIds)
          } else {
            sb.append("\n")
            sb.append(" " * indent2)
            e.prettyPrint(sb, indent2, doSymbolIds)
          }
        case None => Unit
      }
    }
  }

  case class While(cond: ExprTree, body: ExprTree) extends ExprTree {
    override def prettyPrint(sb: StringBuilder, indent: Int, doSymbolIds: Boolean): Unit = {
      sb.append("while (")
      cond.prettyPrint(sb, indent, doSymbolIds)
      sb.append(") ")
      val indent2 = indent + indent_length
      if (body.isInstanceOf[Block]) {
        body.prettyPrint(sb, indent, doSymbolIds)
      } else {
        sb.append("\n")
        sb.append(" " * indent2)
        body.prettyPrint(sb, indent2, doSymbolIds)
      }
    }
  }

  case class Println(expr: ExprTree) extends ExprTree {
    override def prettyPrint(sb: StringBuilder, indent: Int, doSymbolIds: Boolean): Unit = {
      sb.append("println(")
      expr.prettyPrint(sb, indent, doSymbolIds)
      sb.append(")")
    }
  }

  case class Assign(id: Identifier, expr: ExprTree) extends ExprTree {
    override def prettyPrint(sb: StringBuilder, indent: Int, doSymbolIds: Boolean): Unit = {
      id.prettyPrint(sb, indent, doSymbolIds)
      sb.append(" = ")
      expr.prettyPrint(sb, indent, doSymbolIds)
    }
  }

}
