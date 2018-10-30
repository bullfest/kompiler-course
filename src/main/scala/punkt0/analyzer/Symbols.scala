package punkt0
package analyzer

import scala.collection.mutable

object Symbols {

  trait Symbolic[S <: Symbol] {
    private var _sym: Option[S] = None

    def setSymbol(sym: S): this.type = {
      _sym = Some(sym)
      this
    }

    def hasSymbol: Boolean = _sym.isDefined

    def getSymbol: S = _sym match {
      case Some(s) => s
      case None => sys.error("Accessing undefined symbol.")
    }
  }

  sealed abstract class Symbol extends Positioned {
    val id: Int = ID.next
    val name: String
  }

  private object ID {
    private var c: Int = 0

    def next: Int = {
      val ret = c
      c = c + 1
      ret
    }
  }

  class GlobalScope {
    var mainClass: ClassSymbol = _
    var classes: Map[String, ClassSymbol] = Map[String, ClassSymbol]()

    def lookupClass(n: String): Option[ClassSymbol] = classes.get(n)
  }

  class ClassSymbol(val name: String) extends Symbol {
    var parent: Option[ClassSymbol] = None
    var methods: Map[String, MethodSymbol] = Map[String, MethodSymbol]()
    var members: Map[String, VariableSymbol] = Map[String, VariableSymbol]()

    def lookupMethod(n: String): Option[MethodSymbol] = methods.get(n) match {
      case m @ Some(_) => m
      case None => parent match {
        case Some(cls) => cls.lookupMethod(n)
        case None => None
      }
    }

    def lookupVar(n: String): Option[VariableSymbol] = members.get(n) match {
      case v @ Some(_) => v
      case None => parent match {
        case Some(cls) => cls.lookupVar(n)
        case None => None
      }
    }
  }

  class MethodSymbol(val name: String, val classSymbol: ClassSymbol) extends Symbol {
    var params: mutable.LinkedHashMap[String, VariableSymbol] = mutable.LinkedHashMap[String, VariableSymbol]()
    var members: Map[String, VariableSymbol] = Map[String, VariableSymbol]()
    var overridden: Option[MethodSymbol] = None

    def lookupVar(n: String): Option[VariableSymbol] = members.get(n) match {
      case v @ Some(_) => v
      case None =>
        params.get(n) match {
          case v @ Some(_) => v
          case None => classSymbol.lookupVar(n)
        }
    }
  }

  class VariableSymbol(val name: String) extends Symbol

}
