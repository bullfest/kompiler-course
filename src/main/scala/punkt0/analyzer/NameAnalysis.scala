package punkt0
package analyzer

import punkt0.ast.Trees._

object NameAnalysis extends Phase[Program, Program] {

  def unrecognizedIdentError(pos: Positioned): Unit = {
    // Don't terminate, these errors shouldn't really affect each other
    Reporter.error("Unrecognized identifier", pos)
  }

  def illegalOverrideError(pos: Positioned): Unit = {
    // Don't terminate, these errors shouldn't really affect each other
    Reporter.error("Overriding superclass method without override keyword", pos)
  }

  def missingSuperMethodError(pos: Positioned): Unit = {
    // Don't terminate, these errors shouldn't really affect each other
    Reporter.error("There is no method to override in superclass", pos)
  }

  def run(prog: Program)(ctx: Context): Program = {

    val globalScope = new Symbols.GlobalScope

    globalScope.mainClass = prog.main.collectSymbol

    prog.classes.foreach(cls => globalScope.classes += (cls.id.value -> cls.collectSymbol))

    prog.attachSymbols(globalScope)

    // Step 1: Collect symbols in declarations
    // Step 2: Attach symbols to identifiers (except method calls) in method bodies
    // (Step 3:) Print tree with symbol ids for debugging

    // Make sure you check all constraints
    Reporter.terminateIfErrors()
    prog
  }

}
