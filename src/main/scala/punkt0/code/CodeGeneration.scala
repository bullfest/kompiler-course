package punkt0
package code

import cafebabe.AbstractByteCodes.{New => _, _}
import cafebabe.ByteCodes._
import cafebabe._
import punkt0.analyzer.Symbols.{MethodSymbol, VariableSymbol}
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
          field.getSymbol.className = className
          field.getSymbol.isField = true
      }

      val constructorCH = classFile.addConstructor().codeHandler
      ct.vars foreach {
        field =>
          generateCode(constructorCH, field.expr)
          constructorCH << PutField(className, field.id.value, field.tpe.getType.compilerType)
      }
      constructorCH << RETURN
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
      for (i <- mt.args.indices) {
        mt.args(i).getSymbol.compilerVariable = i + 1
      }

      mt.vars foreach { _var =>
        _var.getSymbol.compilerVariable = ch.getFreshVar
        generateCode(ch, _var.expr)
        ch << storeVar(_var.getSymbol)
      }

      mt.exprs.foreach(expr => {
        generateCode(ch, expr)
        if (expr.getType != TUnit)
          ch << POP
      })

      generateCode(ch, mt.retExpr)

      // Return different bytecode depending on retExpr type
      mt.retType.getType match {
        case TInt | TBoolean =>
          ch << IRETURN
        case TUnit =>
          ch << RETURN
        case _ =>
          // All the rest should be class types
          ch << ARETURN
      }

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
            ICONST_1 <<
            Goto(afterLabel)
          // False
          ch <<
            Label(falseLabel) <<
            ICONST_0

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
            ICONST_0 <<
            Goto(afterLabel)
          // True
          ch <<
            Label(trueLabel) <<
            ICONST_1

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
            ICONST_0 <<
            Goto(afterLabel)
          // True
          ch <<
            Label(trueLabel) <<
            ICONST_1

          ch <<
            Label(afterLabel)

        case Equals(lhs, rhs) =>
          val trueLabel = ch.getFreshLabel("true")
          val afterLabel = ch.getFreshLabel("after")

          generateCode(ch, lhs)
          generateCode(ch, rhs)
          lhs.getType match {
            case TNull | TString | _: TClass =>
              ch << If_ACmpEq(trueLabel)
            case TInt | TBoolean =>
              ch << If_ICmpEq(trueLabel)
            case TUnit =>
              // Comparing 2 Unit-values should always be true.
              ch << ICONST_1
              return
            case _ => sys.error("This shouldn't happen")
          }

          // False
          ch <<
            ICONST_0 <<
            Goto(afterLabel)
          // True
          ch <<
            Label(trueLabel) <<
            ICONST_1

          ch <<
            Label(afterLabel)

        case MethodCall(obj, meth, args) =>
          generateCode(ch, obj)
          args.foreach((exprTree: ExprTree) => generateCode(ch, exprTree))
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
        case ident: Identifier =>
          val symbol = ident.getSymbol.asInstanceOf[VariableSymbol]
          if (symbol.isField) {
            //It's a field
            ch << GetField(symbol.className, ident.value, ident.getType.compilerType)
          } else {
            //It's a local variable or a parameter
            if (ident.getType.eq(TBoolean) || ident.getType.eq(TInt))
              ch << ILoad(symbol.compilerVariable)
            else
              ch << ALoad(symbol.compilerVariable)
          }
        case This() =>
          ch << ArgLoad(0)
        case Null() =>
          ch << ACONST_NULL
        case New(tpe) =>
          ch << DefaultNew(tpe.value)
        case Not(expr) =>
          val trueLabel = ch.getFreshLabel("true")
          val afterLabel = ch.getFreshLabel("after")
          generateCode(ch, expr)
          ch << IfEq(trueLabel)
          ch <<
            ICONST_0 <<
            Goto(afterLabel)
          ch <<
            Label(trueLabel) <<
            ICONST_1
          ch <<
            Label(afterLabel)

        case Block(exprs) =>
          exprs.zipWithIndex.foreach(pair => {
            val expr = pair._1
            val index = pair._2
            generateCode(ch, expr)
            if (index < exprs.size - 1 && expr.getType != TUnit)
              ch << POP //remove value left on the stack
          })

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
          val conditionLabel = ch.getFreshLabel("conditionLabel")
          val afterLabel = ch.getFreshLabel("afterLabel")
          ch << Label(conditionLabel)
          generateCode(ch, cond)
          ch << IfEq(afterLabel)
          generateCode(ch, body)
          ch << Goto(conditionLabel)
          ch << Label(afterLabel)

        case Println(expr) =>
          ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
          generateCode(ch, expr)
          ch << InvokeVirtual("java/io/PrintStream", "println", s"(${expr.getType.compilerType})V")
        case Assign(id, expr) =>
          generateCode(ch, expr)
          ch << storeVar(id.getSymbol.asInstanceOf[VariableSymbol])
      }
    }

    def storeVar(symbol: VariableSymbol): AbstractByteCodeGenerator = {
      symbol.getType match {
        case _: TClass | TNull | TString =>
          AStore(symbol.compilerVariable)
        case TInt | TBoolean =>
          IStore(symbol.compilerVariable)
        case _ => sys.error("This shouldn't happen")
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

    val mainClass = new cafebabe.ClassFile("Main")
    mainClass.setSourceFile(sourceName)

    val codeHandler = mainClass.addMainMethod.codeHandler

    prog.main.vars.foreach(var_ => {
      var_.getSymbol.compilerVariable = codeHandler.getFreshVar
      generateCode(codeHandler, var_.expr)
      codeHandler << storeVar(var_.getSymbol)
    })

    prog.main.exprs foreach { expr =>
      generateCode(codeHandler, expr)
      if (expr.getType != TUnit)
        codeHandler << POP //remove value left on the stack
    }
    codeHandler << RETURN

    codeHandler.freeze
    mainClass.writeToFile(outDir + "Main.class")
  }

}
