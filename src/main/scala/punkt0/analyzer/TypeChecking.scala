package punkt0
package analyzer

import ast.Trees._

import Symbols._
import Types._

object TypeChecking extends Phase[Program, Program] {

  /** Typechecking does not produce a value, but has the side effect of
   * attaching types to trees and potentially outputting error messages. */
  def run(prog: Program)(ctx: Context): Program = {

    def tcTree(tree: Tree): Unit = tree match {
      case Program(main, classes) =>
        tcTree(main)
        classes.foreach(tcTree)
      case MainDecl(obj, parent, vars, exprs) =>
        if (parent.getSymbol.name != "App")
          Reporter.error("Main declaration must extend \"App\"", tree)
        vars.foreach(tcTree)
        exprs.foreach(tcExpr(_))
      case ClassDecl(id, parent, vars, methods) =>
      case VarDecl(tpe, id, expr) =>
      case MethodDecl(overrides, retType, id, args, vars, exprs, retExpr) =>
      case _ => sys.error("This should not be able to happen")
    }

    def tcOperator(operator: BinaryOperator): Type = {
      operator match {
        case Plus(lhs, rhs) =>
          val t1 = tcExpr(lhs, TInt, TString)
          val t2 = tcExpr(rhs, TInt, TString)
          if (t1 == TString || t2 == TString)
            TString
          else
            TInt
        case Equals(lhs, rhs) =>
          val t1 = tcExpr(lhs)
          val t2 = tcExpr(rhs)
          if ((t1.isSubTypeOf(TNull) && t2.isSubTypeOf(TNull)) || t1 == t2)
            TBoolean
          else
            TError
        case And(lhs, rhs) =>
          tcExpr(lhs, TBoolean)
          tcExpr(rhs, TBoolean)
          TBoolean
        case Or(lhs, rhs) =>
          tcExpr(lhs, TBoolean)
          tcExpr(rhs, TBoolean)
          TBoolean
        case Minus(lhs, rhs) =>
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
          TInt
        case Times(lhs, rhs) =>
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
          TInt
        case Div(lhs, rhs) =>
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
          TInt
        case LessThan(lhs, rhs) =>
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
          TBoolean
      }
    }

    def tcExpr(expr: ExprTree, expected: Type*): Type = {
      val tpe: Type = expr match {
        case operator: BinaryOperator =>
          tcOperator(operator)
        case MethodCall(obj, meth, args) =>
        case IntLit(value) =>
          TInt
        case StringLit(value) =>
          TString
        case True() =>
          TBoolean
        case False() =>
          TBoolean
        case i: Identifier =>
          i.getType
        case t: This =>
          t.getSymbol.getType
        case Null() =>
          TNull
        case New(tpe) =>
          tpe.getType
        case Not(expr) =>
          tcExpr(expr, TBoolean)
          TBoolean
        case Block(exprs) =>
          var lastType: Type = null
          for (e <- exprs) {
            lastType = tcExpr(e)
          }
          lastType
        case If(cond, thn, els) =>
          tcExpr(cond, TBoolean)
          val t1 = tcExpr(thn)
          els match {
            case Some(els_) =>
              val t2 = tcExpr(els_)
              if (t1.isSubTypeOf(t2))
                t2
              else if (t2.isSubTypeOf(t1))
                t1
              else
                TError
            case None =>
              TUnit
          }
        case While(cond, body) =>
          tcExpr(cond, TBoolean)
          tcExpr(body, TUnit)
          TUnit
        case Println(expr) =>
          tcExpr(expr, TString, TInt, TBoolean)
          TUnit
        case Assign(id, expr) =>
          val t1 = tcExpr(expr)
          if (!t1.isSubTypeOf(id.getType))
            Reporter.error(t1 + "is not a subtype of " + id.getType, expr)
          TUnit
      }
      expr.setType(tpe)

      if (tpe == TError)
        Reporter.error("Type error: Bad type", expr)
      // Check result and return a valid type in case of error
      if (expected.isEmpty) {
        tpe
      } else if (!expected.exists(e => tpe.isSubTypeOf(e))) {
        Reporter.error("Type error: expected: " + expected.toList.mkString(" or ") + ", found: " + tpe, expr)
        expected.head
      } else {
        tpe
      }
    }

    tcTree(prog)
    prog
  }

}
