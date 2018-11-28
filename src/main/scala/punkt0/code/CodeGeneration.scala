package punkt0
package code

import ast.Trees._
import analyzer.Symbols._
import analyzer.Types._
import cafebabe._
import AbstractByteCodes.{New => _, _}
import ByteCodes._

object CodeGeneration extends Phase[Program, Unit] {

  def run(prog: Program)(ctx: Context): Unit = {

    /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
    def generateClassFile(sourceName: String, ct: ClassDecl, dir: String): Unit = {
      val className = ct.id.value
      val classFile = ct.parent match {
        case Some(parent) =>
          new cafebabe.ClassFile(className, Some(parent.value))
        case None =>
          new cafebabe.ClassFile(className, None)
      }
      classFile.setSourceFile(sourceName)

      ct.vars foreach {
        field =>
          classFile.addField(field.tpe.getType.compilerType, field.id.value)
      }

      val constructorCH = classFile.addConstructor().codeHandler
      ct.vars foreach {
        field =>
          generateCode(constructorCH,field.expr)
          constructorCH << PutField(className, field.id.value, field.tpe.getType.compilerType)
      }
      constructorCH.freeze

      ct.methods foreach {
        method =>
          val codeHandler = classFile.addMethod(
            method.retType.getType.compilerType,
            method.id.value,
            method.args.map(_.tpe.getType.compilerType)
          ).codeHandler
          generateMethodCode(codeHandler, method)
      }

      classFile.writeToFile(dir + className + ".class")
    }

    // a mapping from variable symbols to positions in the local variables
    // of the stack frame
    def generateMethodCode(ch: CodeHandler, mt: MethodDecl): Unit = {
      val methSym = mt.getSymbol

      // TODO: Emit code

      ch.freeze
    }

    def generateCode(ch: CodeHandler, exprTree: ExprTree): Unit = {
      exprTree match {
        case And(lhs, rhs) =>
        case Or(lhs, rhs) =>
        case Plus(lhs, rhs) =>
        case Minus(lhs, rhs) =>
        case Times(lhs, rhs) =>
        case Div(lhs, rhs) =>
        case LessThan(lhs, rhs) =>
        case Equals(lhs, rhs) =>
        case MethodCall(obj, meth, args) =>
        case IntLit(value) =>
        case StringLit(value) =>
        case True() =>
        case False() =>
        case Identifier(value) =>
        case This() =>
        case Null() =>
        case New(tpe) =>
        case Not(expr) =>
        case Block(exprs) =>
        case If(cond, thn, els) =>
        case While(cond, body) =>
        case Println(expr) =>
        case Assign(id, expr) =>
      }
    }

    val outDir = ctx.outDir.map(_.getPath + "/").getOrElse("./")

    val f = new java.io.File(outDir)
    if (!f.exists()) {
      f.mkdir()
    }

    val sourceName = ctx.file.get.getName

    // output code
    prog.classes foreach {
      ct => generateClassFile(sourceName, ct, outDir)
    }

    // Now do the main declaration
    // ...
  }

}
