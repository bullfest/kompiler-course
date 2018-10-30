package punkt0

import java.io.File

import lexer._
import punkt0.analyzer.NameAnalysis
import punkt0.ast.{Parser, Printer}


object Main {

  def processOptions(args: Array[String]): Context = {
    var ctx = Context()

    def processOption(args: List[String]): Unit = args match {
      case "--help" :: argsTail =>
        ctx = ctx.copy(doHelp = true)
        processOption(argsTail)

      case "-d" :: out :: argsTail =>
        ctx = ctx.copy(outDir = Some(new File(out)))
        processOption(argsTail)

      case "--tokens" :: argsTail =>
        ctx = ctx.copy(doTokens = true)
        processOption(argsTail)

      case "--ast" :: argsTail =>
        ctx = ctx.copy(doAST = true)
        processOption(argsTail)

      case "--print" :: argsTail =>
        ctx = ctx.copy(doPrintMain = true)
        processOption(argsTail)

      case "--symid" :: argsTail =>
        ctx = ctx.copy(doSymbolIds = true)
        processOption(argsTail)

      case f :: argsTail =>
        ctx = ctx.copy(file = Some(new File(f)))
        processOption(argsTail)

      case List() =>
    }

    processOption(args.toList)

    if (ctx.doHelp) {
      displayHelp()
      sys.exit(0)
    }

    if (ctx.doTokens) {
      for (token <- Lexer.run(ctx.file.get)(ctx))
        print(token)
      Reporter.terminateIfErrors()
      sys.exit(0)
    }

    if (ctx.doAST) {
      val result = Lexer.andThen(Parser).run(ctx.file.get)(ctx)
      print(result)
      sys.exit(0)
    }

    if (ctx.doPrintMain) {
      Lexer.andThen(Parser).andThen(Printer).run(ctx.file.get)(ctx)
      sys.exit(0)
    }

    if (ctx.doSymbolIds) {
      Lexer.andThen(Parser).andThen(NameAnalysis).andThen(Printer).run(ctx.file.get)(ctx)
      sys.exit(0)
    }

    ctx
  }

  def displayHelp(): Unit = {
    println("Usage: <punkt0c> [options] <file>")
    println("Options include:")
    println(" --help        displays this help")
    println(" -d <outdir>   generates class files in the specified directory")
  }

  def main(args: Array[String]): Unit = {
    val ctx = processOptions(args)

    val result = Lexer.andThen(Parser).run(ctx.file.get)(ctx)
  }
}
