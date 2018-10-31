package punkt0
package ast

import punkt0.analyzer.NameAnalysis
import punkt0.analyzer.Symbols._

object Trees {
  val indent_length = 2

  def attachSymbolsType(tpe: TypeTree, globalScope: GlobalScope): Unit = {
    tpe match {
      case ident: Identifier =>
        globalScope.lookupClass(ident.value) match {
          case Some(symbol) =>
            ident.setSymbol(symbol)
          case None =>
            NameAnalysis.unrecognizedIdentError(ident)
        }
      case _ =>
    }
  }

  sealed trait Tree extends Positioned {
    def prettyPrint(sb: StringBuilder, indent: Int, doSymbolIds: Boolean): Unit

    def attachSymbols(gs: GlobalScope, classScope: ClassSymbol = null, methodScope: MethodSymbol = null): Unit = Unit
  }

  sealed trait TypeTree extends Tree

  sealed trait ExprTree extends Tree

  abstract sealed class BinaryOperator(lhs: ExprTree, rhs: ExprTree, operator: String) extends ExprTree {

    override def attachSymbols(gs: GlobalScope, classScope: ClassSymbol, methodScope: MethodSymbol): Unit = {
      lhs.attachSymbols(gs, classScope, methodScope)
      rhs.attachSymbols(gs, classScope, methodScope)
    }

    override def prettyPrint(sb: StringBuilder, indent: Int, doSymbolIds: Boolean): Unit = {
      lhs.prettyPrint(sb, indent, doSymbolIds)
      sb.append(" " + operator + " ")
      rhs.prettyPrint(sb, indent, doSymbolIds)
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

    override def attachSymbols(gs: GlobalScope, classScope: ClassSymbol, methodScope: MethodSymbol): Unit = {
      attachSymbolsType(tpe, gs)
    }
  }

  case class Program(main: MainDecl, classes: List[ClassDecl]) extends Tree {
    override def prettyPrint(sb: StringBuilder, indent: Int, doSymbolIds: Boolean): Unit = {
      classes.foreach(c => {
        c.prettyPrint(sb, indent, doSymbolIds)
        sb.append("\n\n")
      })
      main.prettyPrint(sb, indent, doSymbolIds)
      sb.append("\n")
    }

    override def attachSymbols(gs: GlobalScope, classSymbol: ClassSymbol, methodSymbol: MethodSymbol): Unit = {
      main.attachSymbols(gs)
      classes.foreach(_.attachSymbols(gs))
    }
  }

  case class MainDecl(obj: Identifier, parent: Identifier, vars: List[VarDecl], exprs: List[ExprTree]) extends Tree with Symbolic[ClassSymbol] {

    def collectSymbol: ClassSymbol = {
      val symbol = new ClassSymbol(obj.value)
      setSymbol(symbol)
      obj.setSymbol(symbol)
      for (variable <- vars) {
        val varSymbol = variable.collectSymbol
        symbol.members.get(varSymbol.name) match {
          case Some(_) =>
            NameAnalysis.multipleDeclarationError(variable)
          case None =>
            symbol.members += (varSymbol.name -> varSymbol)
        }
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

    override def attachSymbols(gs: GlobalScope, classScope: ClassSymbol, methodScope: MethodSymbol): Unit = {
      gs.lookupClass(parent.value) match {
        case Some(symbol) =>
          parent.setSymbol(symbol)
          getSymbol.parent = Some(symbol)
        case None => NameAnalysis.unrecognizedIdentError(parent)
      }
      vars.foreach(_.attachSymbols(gs, getSymbol))
      exprs.foreach(_.attachSymbols(gs, getSymbol))
    }
  }

  case class ClassDecl(id: Identifier, parent: Option[Identifier], vars: List[VarDecl], methods: List[MethodDecl]) extends Tree with Symbolic[ClassSymbol] {

    def collectSymbol: ClassSymbol = {
      val symbol = new ClassSymbol(id.value)
      setSymbol(symbol)
      id.setSymbol(symbol)
      for (variable <- vars) {
        val varSymbol = variable.collectSymbol
        symbol.lookupVar(varSymbol.name) match {
          case Some(_) =>
            NameAnalysis.multipleDeclarationError(variable)
          case None =>
            symbol.members += (varSymbol.name -> varSymbol)
        }
      }
      for (method <- methods) {
        val methodSymbol = method.collectSymbol(symbol)
        symbol.lookupMethod(symbol.name) match {
          case Some(_) =>
            NameAnalysis.multipleDeclarationError(method)
          case None =>
            symbol.lookupVar(symbol.name) match {
              case Some(_) =>
                NameAnalysis.multipleDeclarationError(method)
              case None =>
                symbol.methods += (methodSymbol.name -> methodSymbol)
            }
        }
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

    override def attachSymbols(gs: GlobalScope, classScope: ClassSymbol, methodScope: MethodSymbol): Unit = {
      parent match {
        case Some(parent_) =>
          gs.lookupClass(parent_.value) match {
            case Some(symbol) =>
              parent_.setSymbol(symbol)
              getSymbol.parent = Some(symbol)
            case None => NameAnalysis.unrecognizedIdentError(parent_)
          }
        case None =>
      }
      if (getSymbol.isSubclassOf(getSymbol.name))
        NameAnalysis.inheritanceCycleError(this)

      vars.foreach(_.attachSymbols(gs, getSymbol))
      methods.foreach(_.attachSymbols(gs, getSymbol))
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

    override def attachSymbols(gs: GlobalScope, classScope: ClassSymbol, methodScope: MethodSymbol): Unit = {
      if (methodScope == null) {
        //It's a class variable
        if (classScope == null)
          sys.error("Bad scope")
        classScope.parent match {
          case Some(parent) =>
            parent.lookupVar(id.value) match {
              case Some(value) =>
                NameAnalysis.multipleDeclarationError(id)
              case None =>
            }
          case None =>
        }
      }
      attachSymbolsType(tpe, gs)
      expr.attachSymbols(gs, classScope, methodScope)
    }
  }

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

  case class MethodDecl(overrides: Boolean, retType: TypeTree, id: Identifier,
                        args: List[Formal], vars: List[VarDecl], exprs: List[ExprTree],
                        retExpr: ExprTree) extends Tree with Symbolic[MethodSymbol] {

    def collectSymbol(cls: ClassSymbol): MethodSymbol = {
      val symbol = new MethodSymbol(id.value, cls)
      id.setSymbol(symbol)
      setSymbol(symbol)
      for (arg <- args) {
        val argSymbol = arg.collectSymbol
        symbol.params.get(argSymbol.name) match {
          case Some(_) =>
            NameAnalysis.multipleDeclarationError(arg)
          case None =>
            symbol.params += (argSymbol.name -> argSymbol)
        }
      }
      for (var_ <- vars) {
        val varSymbol = var_.collectSymbol
        symbol.params.get(varSymbol.name) match {
          case Some(_) =>
            NameAnalysis.multipleDeclarationError(var_)
          case None =>
            symbol.members.get(varSymbol.name) match {
              case Some(_) =>
                NameAnalysis.multipleDeclarationError(var_)
              case None =>
                symbol.members += (varSymbol.name -> varSymbol)
            }
        }
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

    override def attachSymbols(gs: GlobalScope, classScope: ClassSymbol, methodScope: MethodSymbol): Unit = {
      classScope.parent match {
        case Some(parent_) =>
          parent_.lookupMethod(id.value) match {
            case Some(otherMethod) =>
              if (!overrides) {
                NameAnalysis.illegalOverrideError(this)
              } else {
                if (otherMethod.params.size != getSymbol.params.size) {
                  NameAnalysis.nonMatchingParamsError(this)
                }
              }
            case None =>
              if (overrides) {
                NameAnalysis.missingSuperMethodError(this)
              }
          }
        case None =>
          if (overrides) {
            NameAnalysis.missingSuperMethodError(this)
          }
      }
      attachSymbolsType(retType, gs)
      args.foreach(_.attachSymbols(gs))
      vars.foreach(_.attachSymbols(gs, classScope, getSymbol))
      exprs.foreach(_.attachSymbols(gs, classScope, getSymbol))
      retExpr.attachSymbols(gs, classScope, getSymbol)
    }
  }

  case class And(lhs: ExprTree, rhs: ExprTree) extends BinaryOperator(lhs, rhs, "&&")

  case class Or(lhs: ExprTree, rhs: ExprTree) extends BinaryOperator(lhs, rhs, "||")

  case class Plus(lhs: ExprTree, rhs: ExprTree) extends BinaryOperator(lhs, rhs, "+")

  case class Minus(lhs: ExprTree, rhs: ExprTree) extends BinaryOperator(lhs, rhs, "-")

  case class Times(lhs: ExprTree, rhs: ExprTree) extends BinaryOperator(lhs, rhs, "*")

  case class Div(lhs: ExprTree, rhs: ExprTree) extends BinaryOperator(lhs, rhs, "/")

  case class LessThan(lhs: ExprTree, rhs: ExprTree) extends BinaryOperator(lhs, rhs, "<")

  case class Equals(lhs: ExprTree, rhs: ExprTree) extends BinaryOperator(lhs, rhs, "==")

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

    override def attachSymbols(gs: GlobalScope, classScope: ClassSymbol, methodScope: MethodSymbol): Unit = {
      obj.attachSymbols(gs, classScope, methodScope)
      // meth.attachSymbols(gs, obj.type.getSymbol) // TODO when typechecking is implemented
      args.foreach(_.attachSymbols(gs, classScope, methodScope))
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

    override def attachSymbols(gs: GlobalScope, classScope: ClassSymbol, methodScope: MethodSymbol): Unit = {
      if (methodScope != null) {
        methodScope.lookupVar(value) match {
          case Some(symbol) =>
            setSymbol(symbol)
          case None =>
            NameAnalysis.unrecognizedIdentError(this)
        }
        return
      } else if (classScope != null) {
        classScope.lookupVar(value) match {
          case Some(symbol) =>
            setSymbol(symbol)
          case None =>
            NameAnalysis.unrecognizedIdentError(this)
        }
        return
      }
      // throw real error with stacktrace
      sys.error("Bad scope")
    }
  }

  case class This() extends ExprTree with Symbolic[ClassSymbol] {
    override def prettyPrint(sb: StringBuilder, indent: Int, doSymbolIds: Boolean): Unit = {
      if (doSymbolIds) {
        sb.append("this#")
        if (hasSymbol) {
          sb.append(getSymbol.name).append("#").append(getSymbol.id)
        } else {
          sb.append("??")
        }
      } else {
        sb.append("this")
      }
    }

    override def attachSymbols(gs: GlobalScope, classScope: ClassSymbol, methodScope: MethodSymbol): Unit = {
      if (classScope != null) {
        setSymbol(classScope)
        return
      }
      sys.error("Bad scope")
    }
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

    override def attachSymbols(gs: GlobalScope, classScope: ClassSymbol, methodScope: MethodSymbol): Unit = {
      attachSymbolsType(tpe, gs)
    }
  }

  case class Not(expr: ExprTree) extends ExprTree {
    override def prettyPrint(sb: StringBuilder, indent: Int, doSymbolIds: Boolean): Unit = {
      sb.append("!")
      expr.prettyPrint(sb, indent, doSymbolIds)
    }

    override def attachSymbols(gs: GlobalScope, classScope: ClassSymbol, methodScope: MethodSymbol): Unit = {
      expr.attachSymbols(gs, classScope, methodScope)
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

    override def attachSymbols(gs: GlobalScope, classScope: ClassSymbol, methodScope: MethodSymbol): Unit = {
      exprs.foreach(_.attachSymbols(gs, classScope, methodScope))
    }
  }

  case class If(cond: ExprTree, thn: ExprTree, els: Option[ExprTree]) extends ExprTree {
    override def prettyPrint(sb: StringBuilder, indent: Int, doSymbolIds: Boolean): Unit = {
      sb.append("if (")
      cond.prettyPrint(sb, indent, doSymbolIds)
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

    override def attachSymbols(gs: GlobalScope, classScope: ClassSymbol, methodScope: MethodSymbol): Unit = {
      cond.attachSymbols(gs, classScope, methodScope)
      thn.attachSymbols(gs, classScope, methodScope)
      els match {
        case Some(els_) =>
          els_.attachSymbols(gs, classScope, methodScope)
        case None =>
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

    override def attachSymbols(gs: GlobalScope, classScope: ClassSymbol, methodScope: MethodSymbol): Unit = {
      cond.attachSymbols(gs, classScope, methodScope)
      body.attachSymbols(gs, classScope, methodScope)
    }
  }

  case class Println(expr: ExprTree) extends ExprTree {
    override def prettyPrint(sb: StringBuilder, indent: Int, doSymbolIds: Boolean): Unit = {
      sb.append("println(")
      expr.prettyPrint(sb, indent, doSymbolIds)
      sb.append(")")
    }

    override def attachSymbols(gs: GlobalScope, classScope: ClassSymbol, methodScope: MethodSymbol): Unit = {
      expr.attachSymbols(gs, classScope, methodScope)
    }
  }

  case class Assign(id: Identifier, expr: ExprTree) extends ExprTree {
    override def prettyPrint(sb: StringBuilder, indent: Int, doSymbolIds: Boolean): Unit = {
      id.prettyPrint(sb, indent, doSymbolIds)
      sb.append(" = ")
      expr.prettyPrint(sb, indent, doSymbolIds)
    }

    override def attachSymbols(gs: GlobalScope, classScope: ClassSymbol, methodScope: MethodSymbol): Unit = {
      id.attachSymbols(gs, classScope, methodScope)
      expr.attachSymbols(gs, classScope, methodScope)
    }
  }

}
