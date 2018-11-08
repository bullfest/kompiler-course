package punkt0
package analyzer

import ast.Trees._

import Symbols._
import Types._

object TypeChecking extends Phase[Program, Program] {

  /** Typechecking does not produce a value, but has the side effect of
   * attaching types to trees and potentially outputting error messages. */
  def run(prog: Program)(ctx: Context): Program = {

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
        case Block(exprs) =>
        case If(cond, thn, els) =>
        case While(cond, body) =>
        case Println(expr) =>
        case Assign(id, expr) =>


      }
      expr.setType(tpe)

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

    prog
  }

}
