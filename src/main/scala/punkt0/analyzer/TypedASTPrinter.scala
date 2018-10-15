package punkt0
package analyzer

import ast.Trees._

object TypedASTPrinter {
  def apply(t:Tree):String = {
        (t match {
            case Program(main, classes)=>
                "Program(" + apply(main) + ",[" + classes.map(x => apply(x)).mkString(",") +"])"
            case MainDecl(obj, parent, vars, exprs)=>
                "MainDecl(" + apply(obj) + ",[" + vars.map(x => apply(x)).mkString(",") + "],[" + exprs.map(x => apply(x)).mkString(",") + "])"
            case ClassDecl(id, parent, vars, methods)=>
                "ClassDecl(" + apply(id) + ",[" + vars.map(x => apply(x)).mkString(",") + "],["  + methods.map(x => apply(x)).mkString(",") + "])"
            case VarDecl(tpe, id, expr)=>
                "VarDecl(" + apply(tpe)+ "," + apply(id) + "," + apply(expr) + ")"
            case MethodDecl(overrides, retType, Identifier(id), args, vars, exprs, retExpr)=>
                "MethodDecl(" + overrides + "," + apply(retType) + "," + id + ",[" + args.map(x => apply(x)).mkString(",") + "],[" + vars.map(x => apply(x)).mkString(",") + "],[" + exprs.map(x => apply(x)).mkString(",") + "]," + apply(retExpr) + ")"
            case Formal(tpe, id) => "Formal(" + apply(tpe) + "," + apply(id) + ")"
            case BooleanType()=>
                "BooleanType()"
            case IntType()=>
                "IntType()"
            case StringType()=>
                "StringType()"
            case UnitType()=>
                "UnitType()"
            case And(lhs, rhs)=>
                "And(" + apply(lhs) + "," + apply(rhs) + ")"
            case Or(lhs, rhs)=>
                "Or(" + apply(lhs) + "," + apply(rhs) + ")"
            case Plus(lhs, rhs)=>
                "Plus(" + apply(lhs) + "," + apply(rhs) + ")"
            case Minus(lhs, rhs)=>
                "Minus(" + apply(lhs) + "," + apply(rhs) + ")"
            case Times(lhs, rhs)=>
                "Times(" + apply(lhs) + "," + apply(rhs) + ")"
            case Div(lhs, rhs)=>
                "Div(" + apply(lhs) + "," + apply(rhs) + ")"
            case LessThan(lhs, rhs)=>
                "LessThan(" + apply(lhs) + "," + apply(rhs) + ")"
            case Equals(lhs, rhs)=>
                "Equals(" + apply(lhs) + "," + apply(rhs) + ")"
            case MethodCall(obj, Identifier(meth), args)=>
                "MethodCall(" + apply(obj) + "," + meth + ",[" + args.map(x => apply(x)).mkString(",") + "])"
            case IntLit(value)=>
                "IntLit(" + value + ")"
            case StringLit(value)=>
                "StringLit(" + value + ")"
            case True()=>
                "True()"
            case False()=>
                "False()"
            case Identifier(value)=>
                "Identifier(" + value + ")"
            case This()=>
                "This()"
            case Null()=>
                "Null()"
            case New(tpe)=>
                "New(" + apply(tpe) + ")"
            case Not(expr)=>
                "Not(" + apply(expr) + ")"
            case Block(exprs)=>
                "Block([" + exprs.map(x => apply(x)).mkString(",") + "])"
            case If(expr, thn, els)=>
                "If(" + apply(expr) + "," + apply(thn) + (
                    els match {
                        case Some(t) =>  "," + apply(t)
                        case _ => ""
                    }
                ) + ")"
            case While(cond, body)=>
                "While(" + apply(cond) + "," + apply(body) + ")"
            case Println(expr)=>
                "Println(" + apply(expr) + ")"
            case Assign(id, expr)=>
                "Assign(" + apply(id) + "," + apply(expr) + ")"
      }) + (if (t.isInstanceOf[ExprTree]) {":" + t.asInstanceOf[ExprTree].getTypeStr} else {""})
  }
}