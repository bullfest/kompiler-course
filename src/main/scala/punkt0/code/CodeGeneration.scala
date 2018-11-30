package punkt0
package code

import cafebabe.AbstractByteCodes.{New => _, _}
import cafebabe.ByteCodes._
import cafebabe._
import punkt0.analyzer.Symbols.MethodSymbol
import punkt0.analyzer.Types._
import punkt0.ast.Trees._

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
          generateCode(constructorCH, field.expr)
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
          //If either operand is false go to falseLabel
          val falseLabel = ch.getFreshLabel("false")
          val afterLabel = ch.getFreshLabel("after")

          generateCode(ch, lhs)
          ch << IfEq(falseLabel)

          generateCode(ch, rhs)
          ch << IfEq(falseLabel)

          // True
          ch <<
            ILOAD_1 <<
            Goto(afterLabel)
          // False
          ch <<
            Label(falseLabel) <<
            ILOAD_0

          ch <<
            Label(afterLabel)
        case Or(lhs, rhs) =>
          // If either operand is true go to trueLabel
          val trueLabel = ch.getFreshLabel("true")
          val afterLabel = ch.getFreshLabel("after")

          generateCode(ch, lhs)
          ch << IfNe(trueLabel)

          generateCode(ch, rhs)
          ch << IfNe(trueLabel)

          // False
          ch <<
            ILOAD_0 <<
            Goto(afterLabel)
          // True
          ch <<
            Label(trueLabel) <<
            ILOAD_1

          ch <<
            Label(afterLabel)
        case plus@Plus(lhs, rhs) =>
          if (plus.getType == TString) {
            ch << DefaultNew("java/lang/StringBuilder")
            generateCode(ch, lhs)
            ch << InvokeVirtual("java/lang/StringBuilder", "append", s"(${lhs.getType.compilerType})Ljava/lang/StringBuilder;")
            generateCode(ch, rhs)
            ch << InvokeVirtual("java/lang/StringBuilder", "append", s"(${rhs.getType.compilerType})Ljava/lang/StringBuilder;")
            ch << InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;")
          } else {
            generateCode(ch, lhs)
            generateCode(ch, rhs)
            ch << IADD
          }
        case Minus(lhs, rhs) =>
          generateCode(ch, lhs)
          generateCode(ch, rhs)
          ch << ISUB
        case Times(lhs, rhs) =>
          generateCode(ch, lhs)
          generateCode(ch, rhs)
          ch << IMUL
        case Div(lhs, rhs) =>
          generateCode(ch, lhs)
          generateCode(ch, rhs)
          ch << IDIV
        case LessThan(lhs, rhs) =>
          val trueLabel = ch.getFreshLabel("true")
          val afterLabel = ch.getFreshLabel("after")

          generateCode(ch, lhs)
          generateCode(ch, rhs)
          ch << If_ICmpLt(trueLabel)

          // False
          ch <<
            ILOAD_0 <<
            Goto(afterLabel)
          // True
          ch <<
            Label(trueLabel) <<
            ILOAD_1

          ch <<
            Label(afterLabel)

        case Equals(lhs, rhs) =>
          val trueLabel = ch.getFreshLabel("true")
          val afterLabel = ch.getFreshLabel("after")

          generateCode(ch, lhs)
          generateCode(ch, rhs)
          if (TNull.isSubTypeOf(lhs.getType)) //It's a object
            ch << If_ACmpEq(trueLabel)
          else
            ch << If_ICmpEq(trueLabel)

          // False
          ch <<
            ILOAD_0 <<
            Goto(afterLabel)
          // True
          ch <<
            Label(trueLabel) <<
            ILOAD_1

          ch <<
            Label(afterLabel)

        case MethodCall(obj, meth, args) =>
          generateCode(ch, obj)
          args.foreach(generateCode(ch, _))
          ch << InvokeVirtual(
            obj.getType.toString,
            meth.value,
            meth.getSymbol.asInstanceOf[MethodSymbol].getSignature
          )
        case IntLit(value) =>
          ch << Ldc(value)
        case StringLit(value) =>
          ch << Ldc(value)
        case True() =>
          ch << ICONST_1
        case False() =>
          ch << ICONST_0
        case Identifier(value) =>
        case This() =>
        case Null() =>
          ch << ACONST_NULL
        case New(tpe) =>
          ch << DefaultNew(tpe.value)
        case Not(expr) =>
        case Block(exprs) =>
        case If(cond, thn, els) =>
          val elseLabel = ch.getFreshLabel("elseLabel")
          val afterLabel = ch.getFreshLabel("afterLabel")
          generateCode(ch, cond)
          ch << IfEq(elseLabel)
          generateCode(ch, thn)
          els match {
            case Some(value) =>
              ch << Goto(afterLabel)
              ch << Label(elseLabel)
              generateCode(ch, value)
            case None =>
              ch << Label(elseLabel)
          }
          ch << Label(afterLabel)

        case While(cond, body) =>

        case Println(expr) =>
          ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
          generateCode(ch, expr)
          ch << InvokeVirtual("java/io/PrintStream", "println", s"(${expr.getType.compilerType})V")
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
