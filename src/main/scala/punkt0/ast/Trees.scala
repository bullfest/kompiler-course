package punkt0
package ast

import analyzer.Symbols._

object Trees {
  val indent_length = 2

  sealed trait Tree extends Positioned {
    def prettyPrint(sb: StringBuilder, indent: Int): Unit
  }

  case class Program(main: MainDecl, classes: List[ClassDecl]) extends Tree {
    override def prettyPrint(sb: StringBuilder, indent: Int): Unit = {
      classes.foreach(c => {
        c.prettyPrint(sb, indent); sb.append("\n\n")
      })
      main.prettyPrint(sb, indent)
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

    override def prettyPrint(sb: StringBuilder, indent: Int): Unit = {
      sb.append(" " * indent).append("object ").append(obj.value).append(" extends ").append(parent.value).append(" {\n")
      val indent2 = indent + 2
      for (var_ <- vars) {
        sb.append(" " * indent2)
        var_.prettyPrint(sb, indent2)
        sb.append("\n")
      }
      if (exprs.nonEmpty) {
        sb.append(" " * indent2)
        exprs.head.prettyPrint(sb, indent2)
        for (expression <- exprs.tail) {
          sb.append(";\n")
          sb.append(" " * indent2)
          expression.prettyPrint(sb, indent2)
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

    override def prettyPrint(sb: StringBuilder, indent: Int): Unit = {
      sb.append(" " * indent).append("class ").append(id.value)
      parent match {
        case Some(p) => sb.append(" extends ").append(p.value)
        case None => Unit
      }
      sb.append(" {\n")
      val indent2 = indent + indent_length
      for (var_ <- vars) {
        sb.append(" " * indent2)
        var_.prettyPrint(sb, indent2)
        sb.append("\n")
      }
      for (meth <- methods) {
        sb.append(" " * indent2)
        meth.prettyPrint(sb, indent2)
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

    override def prettyPrint(sb: StringBuilder, indent: Int): Unit = {
      sb.append("var ").append(id.value).append(": ")
      tpe.prettyPrint(sb, indent)
      sb.append(" = ")
      expr.prettyPrint(sb, indent)
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
      symbol
    }

    override def prettyPrint(sb: StringBuilder, indent: Int): Unit = {
      if (overrides)
        sb.append("override ")
      sb.append("def ").append(id.value).append("(")
      if (args.nonEmpty) {
        args.head.prettyPrint(sb, indent)
        for (arg <- args.tail) {
          sb.append(", ")
          arg.prettyPrint(sb, indent)
        }
      }
      sb.append("): ")
      retType.prettyPrint(sb, indent)
      sb.append(" = {\n")
      val indent2 = indent + indent_length
      for (var_ <- vars) {
        sb.append(" " * indent2)
        var_.prettyPrint(sb, indent2)
        sb.append("\n")
      }
      for (expression <- exprs) {
        sb.append(" " * indent2)
        expression.prettyPrint(sb, indent2)
        sb.append(";\n")
      }
      sb.dropRight(2).append("\n")
      sb.append(" " * indent2)
      retExpr.prettyPrint(sb, indent2)
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

    override def prettyPrint(sb: StringBuilder, indent: Int): Unit = {
      sb.append(id.value).append(": ")
      tpe.prettyPrint(sb, indent)
    }
  }

  sealed trait TypeTree extends Tree

  case class BooleanType() extends TypeTree {
    override def prettyPrint(sb: StringBuilder, indent: Int): Unit = {
      sb.append("Boolean")
    }
  }

  case class IntType() extends TypeTree {
    override def prettyPrint(sb: StringBuilder, indent: Int): Unit = {
      sb.append("Int")
    }
  }

  case class StringType() extends TypeTree {
    override def prettyPrint(sb: StringBuilder, indent: Int): Unit = {
      sb.append("String")
    }
  }

  case class UnitType() extends TypeTree {
    override def prettyPrint(sb: StringBuilder, indent: Int): Unit =
      sb.append("Unit")
  }


  sealed trait ExprTree extends Tree

  case class And(lhs: ExprTree, rhs: ExprTree) extends ExprTree {
    override def prettyPrint(sb: StringBuilder, indent: Int): Unit = {
      lhs.prettyPrint(sb, indent)
      sb.append(" && ")
      rhs.prettyPrint(sb, indent)
    }
  }

  case class Or(lhs: ExprTree, rhs: ExprTree) extends ExprTree {
    override def prettyPrint(sb: StringBuilder, indent: Int): Unit = {
      lhs.prettyPrint(sb, indent)
      sb.append(" || ")
      rhs.prettyPrint(sb, indent)
    }
  }

  case class Plus(lhs: ExprTree, rhs: ExprTree) extends ExprTree {
    override def prettyPrint(sb: StringBuilder, indent: Int): Unit = {
      lhs.prettyPrint(sb, indent)
      sb.append(" + ")
      rhs.prettyPrint(sb, indent)
    }
  }

  case class Minus(lhs: ExprTree, rhs: ExprTree) extends ExprTree {
    override def prettyPrint(sb: StringBuilder, indent: Int): Unit = {
      lhs.prettyPrint(sb, indent)
      sb.append(" - ")
      rhs.prettyPrint(sb, indent)
    }
  }

  case class Times(lhs: ExprTree, rhs: ExprTree) extends ExprTree {
    override def prettyPrint(sb: StringBuilder, indent: Int): Unit = {
      lhs.prettyPrint(sb, indent)
      sb.append(" * ")
      rhs.prettyPrint(sb, indent)
    }
  }

  case class Div(lhs: ExprTree, rhs: ExprTree) extends ExprTree {
    override def prettyPrint(sb: StringBuilder, indent: Int): Unit = {
      lhs.prettyPrint(sb, indent)
      sb.append(" / ")
      rhs.prettyPrint(sb, indent)
    }
  }

  case class LessThan(lhs: ExprTree, rhs: ExprTree) extends ExprTree {
    override def prettyPrint(sb: StringBuilder, indent: Int): Unit = {
      lhs.prettyPrint(sb, indent)
      sb.append(" < ")
      rhs.prettyPrint(sb, indent)
    }
  }

  case class Equals(lhs: ExprTree, rhs: ExprTree) extends ExprTree {
    override def prettyPrint(sb: StringBuilder, indent: Int): Unit = {
      lhs.prettyPrint(sb, indent)
      sb.append(" == ")
      rhs.prettyPrint(sb, indent)
    }
  }

  case class MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree]) extends ExprTree {
    override def prettyPrint(sb: StringBuilder, indent: Int): Unit = {
      obj.prettyPrint(sb, indent)
      sb.append(".").append(meth.value).append("(")
      if (args.nonEmpty) {
        args.head.prettyPrint(sb, indent)
        for (arg <- args.tail) {
          sb.append(", ")
          arg.prettyPrint(sb, indent)
        }
      }
      sb.append(")")
    }
  }

  case class IntLit(value: Int) extends ExprTree {
    override def prettyPrint(sb: StringBuilder, indent: Int): Unit =
      sb.append(value)
  }

  case class StringLit(value: String) extends ExprTree {
    override def prettyPrint(sb: StringBuilder, indent: Int): Unit =
      sb.append("\"").append(value).append("\"")
  }

  case class True() extends ExprTree {
    override def prettyPrint(sb: StringBuilder, indent: Int): Unit =
      sb.append("true")
  }

  case class False() extends ExprTree {
    override def prettyPrint(sb: StringBuilder, indent: Int): Unit =
      sb.append("false")
  }

  case class Identifier(value: String) extends TypeTree with ExprTree with Symbolic[Symbol] {
    override def prettyPrint(sb: StringBuilder, indent: Int): Unit =
      sb.append(value)
  }

  case class This() extends ExprTree with Symbolic[ClassSymbol] {
    override def prettyPrint(sb: StringBuilder, indent: Int): Unit =
      sb.append("this")
  }

  case class Null() extends ExprTree {
    override def prettyPrint(sb: StringBuilder, indent: Int): Unit =
      sb.append("null")
  }

  case class New(tpe: Identifier) extends ExprTree {
    override def prettyPrint(sb: StringBuilder, indent: Int): Unit =
      sb.append("new ").append(tpe.value).append("()")
  }

  case class Not(expr: ExprTree) extends ExprTree {
    override def prettyPrint(sb: StringBuilder, indent: Int): Unit = {
      sb.append("!")
      expr.prettyPrint(sb, indent)
    }
  }

  case class Block(exprs: List[ExprTree]) extends ExprTree {
    override def prettyPrint(sb: StringBuilder, indent: Int): Unit = {
      if (exprs.isEmpty)
        sb.append("{}")
      else {
        sb.append("{\n")
        val indent2 = indent + indent_length
        sb.append(" " * indent2)
        exprs.head.prettyPrint(sb, indent2)
        exprs.tail.foreach(expr => {
          sb.append("; \n")
          sb.append(" " * indent2)
          expr.prettyPrint(sb, indent2)
        })
        sb.append("\n")
        sb.append(" " * indent).append("}")
      }
    }
  }

  case class If(expr: ExprTree, thn: ExprTree, els: Option[ExprTree]) extends ExprTree {
    override def prettyPrint(sb: StringBuilder, indent: Int): Unit = {
      sb.append("if (")
      expr.prettyPrint(sb, indent)
      sb.append(") ")
      val indent2 = indent + indent_length
      if (thn.isInstanceOf[Block]) {
        thn.prettyPrint(sb, indent2)
      } else {
        sb.append("\n")
        sb.append(" " * indent2)
        thn.prettyPrint(sb, indent2)
      }
      els match {
        case Some(e) =>
          sb.append(" else ")
          if (thn.isInstanceOf[Block]) {
            e.prettyPrint(sb, indent2)
          } else {
            sb.append("\n")
            sb.append(" " * indent2)
            e.prettyPrint(sb, indent2)
          }
        case None => Unit
      }
    }
  }

  case class While(cond: ExprTree, body: ExprTree) extends ExprTree {
    override def prettyPrint(sb: StringBuilder, indent: Int): Unit = {
      sb.append("while (")
      cond.prettyPrint(sb, indent)
      sb.append(") ")
      val indent2 = indent + indent_length
      if (body.isInstanceOf[Block]) {
        body.prettyPrint(sb, indent)
      } else {
        sb.append("\n")
        sb.append(" " * indent2)
        body.prettyPrint(sb, indent2)
      }
    }
  }

  case class Println(expr: ExprTree) extends ExprTree {
    override def prettyPrint(sb: StringBuilder, indent: Int): Unit = {
      sb.append("println(")
      expr.prettyPrint(sb, indent)
      sb.append(")")
    }
  }

  case class Assign(id: Identifier, expr: ExprTree) extends ExprTree {
    override def prettyPrint(sb: StringBuilder, indent: Int): Unit = {
      sb.append(id.value)
      sb.append(" = ")
      expr.prettyPrint(sb, indent)
    }
  }

}
