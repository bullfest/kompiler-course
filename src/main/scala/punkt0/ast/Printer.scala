package punkt0
package ast

import Trees._

object Printer extends Phase [Program, Unit]{
  def run(t: Program)(ctx: Context): Unit = {
    val sb = new StringBuilder()
    t.prettyPrint(sb, 0)
    println(sb.toString())
  }
}
