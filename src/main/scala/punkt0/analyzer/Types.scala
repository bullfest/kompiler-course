package punkt0
package analyzer

import punkt0.analyzer.Symbols._

object Types {

  trait Typed {
    private var _tpe: Type = TUntyped

    def setType(tpe: Type): this.type = { _tpe = tpe; this }
    def getType: Type = _tpe
    def getTypeStr: String = {
      try getType.toString
      catch {
        case t: Throwable => "??"
      }
    }
  }

  sealed abstract class Type {
    def isSubTypeOf(tpe: Type): Boolean
  }

  // special object to implement the fact that all objects are its subclasses
  val anyRef = TClass(new ClassSymbol("AnyRef"))

  case class TClass(classSymbol: ClassSymbol) extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = {
      tpe match {
        case TClass(otherClassSymbol) =>
          classSymbol.isSubclassOf(otherClassSymbol.name)
        case _ => false
      }
    }

    override def toString: String = classSymbol.name
  }

  case object TError extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = true

    override def toString: String = "[error]"
  }

  case object TUntyped extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = false

    override def toString: String = "[untyped]"
  }

  case object TInt extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TInt => true
      case _ => false
    }

    override def toString: String = "Int"
  }

  case object TBoolean extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TBoolean => true
      case _ => false
    }

    override def toString: String = "Boolean"
  }

  case object TString extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TString => true
      case _ => false
    }

    override def toString: String = "String"
  }

  case object TUnit extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TUnit => true
      case _ => false
    }

    override def toString: String = "Unit"
  }
}
