package punkt0
package ast

import Trees._

object Printer {
  def apply(t: Tree): String = {
    val sb = new StringBuilder()
    t.prettyPrint(sb, 0)
    sb.toString()
  }
}
