package punkt0
package ast

object Trees {
  sealed trait Tree extends Positioned {
    def prettyPrint(sb: StringBuilder, indent: Int): Unit
  }

  case class Program(main: MainDecl, classes: List[ClassDecl]) extends Tree {
    override def prettyPrint(sb: StringBuilder, indent: Int): Unit = {
      classes.foreach(c => {c.prettyPrint(sb, indent); sb.append("\n\n")})
      main.prettyPrint(sb, indent)
    }
  }

  case class MainDecl(obj: Identifier, parent: Identifier, vars: List[VarDecl], exprs: List[ExprTree]) extends Tree {
    override def prettyPrint(sb: StringBuilder, indent: Int): Unit= {
      sb.append(" "*indent).append("object ").append(obj.value).append(" extends ").append(parent.value).append(" {\n")
      val indent2 = indent + 2
      for (var_ <- vars) {
        sb.append(" "*indent2)
        var_.prettyPrint(sb, indent2)
        sb.append("\n")
      }
      for (expression <- exprs) {
        sb.append(" "*indent2)
        expression.prettyPrint(sb, indent2)
        sb.append(";\n")
      }
      sb.dropRight(2).append("\n")
      sb.append(" "*indent).append("}")
    }
  }

  case class ClassDecl(id: Identifier, parent: Option[Identifier], vars: List[VarDecl], methods: List[MethodDecl]) extends Tree {
    override def prettyPrint(sb: StringBuilder, indent: Int): Unit= {
      sb.append(" "*indent).append("class ").append(id.value)
      parent match {
        case Some(p) => sb.append(" extends ").append(p.value)
        case None => Unit
      }
      sb.append(" {\n")
      val indent2 = indent + 2
      for (var_ <- vars) {
        sb.append(" "*indent2)
        var_.prettyPrint(sb, indent2)
        sb.append("\n")
      }
      for (meth <- methods) {
        sb.append(" " * indent2)
        meth.prettyPrint(sb, indent2)
        sb.append("\n")
      }
      sb.append(" "*indent).append("}")
    }
  }
  case class VarDecl(tpe: TypeTree, id: Identifier, expr: ExprTree) extends Tree {
    override def prettyPrint(sb: StringBuilder, indent: Int): Unit= {
      sb.append("var ").append(id.value).append(": ")
      tpe.prettyPrint(sb, indent)
      sb.append(" = ")
      expr.prettyPrint(sb, indent)
      sb.append(";")
    }
  }
  case class MethodDecl(overrides: Boolean, retType: TypeTree, id: Identifier, args: List[Formal], vars: List[VarDecl], exprs: List[ExprTree], retExpr: ExprTree) extends Tree  {
    override def prettyPrint(sb: StringBuilder, indent: Int): Unit= {
      if (overrides)
        sb.append("overrides ")
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
      sb.append(" = {")
      val indent2 = indent + 2
      for (var_ <- vars) {
        sb.append(" "*indent2)
        var_.prettyPrint(sb, indent2)
        sb.append("\n")
      }
      for (expression <- exprs) {
        sb.append(" "*indent2)
        expression.prettyPrint(sb, indent2)
        sb.append(";\n")
      }
      sb.dropRight(2).append("\n")
      sb.append(" "* indent2)
      retExpr.prettyPrint(sb, indent2)
      sb.append(" "*indent).append("}")
    }
  }
  sealed case class Formal(tpe: TypeTree, id: Identifier) extends Tree

  sealed trait TypeTree extends Tree
  case class BooleanType() extends TypeTree
  case class IntType() extends TypeTree
  case class StringType() extends TypeTree
  case class UnitType() extends TypeTree

  sealed trait ExprTree extends Tree
  case class And(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Or(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Plus(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Minus(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Times(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Div(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class LessThan(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Equals(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree]) extends ExprTree
  case class IntLit(value: Int) extends ExprTree
  case class StringLit(value: String) extends ExprTree

  case class True() extends ExprTree
  case class False() extends ExprTree
  case class Identifier(value: String) extends TypeTree with ExprTree
  case class This() extends ExprTree
  case class Null() extends ExprTree
  case class New(tpe: Identifier) extends ExprTree
  case class Not(expr: ExprTree) extends ExprTree

  case class Block(exprs: List[ExprTree]) extends ExprTree
  case class If(expr: ExprTree, thn: ExprTree, els: Option[ExprTree]) extends ExprTree
  case class While(cond: ExprTree, body: ExprTree) extends ExprTree
  case class Println(expr: ExprTree) extends ExprTree
  case class Assign(id: Identifier, expr: ExprTree) extends ExprTree
}
