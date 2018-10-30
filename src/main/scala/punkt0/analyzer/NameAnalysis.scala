package punkt0
package analyzer

import ast.Trees._

object NameAnalysis extends Phase[Program, Program] {

  def run(prog: Program)(ctx: Context): Program = {
    import Reporter._

    val globalScope = new Symbols.GlobalScope

    globalScope.mainClass = prog.main.collectSymbol

    prog.classes.foreach(cls => globalScope.classes += (cls.id.value -> cls.collectSymbol))

    // Step 1: Collect symbols in declarations
    // Step 2: Attach symbols to identifiers (except method calls) in method bodies
    // (Step 3:) Print tree with symbol ids for debugging

    // Make sure you check all constraints
    prog
  }

}
