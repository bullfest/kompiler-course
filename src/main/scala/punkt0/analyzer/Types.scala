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
    val compilerType: String
    def isSubTypeOf(tpe: Type): Boolean
  }

  // special object to implement the fact that all objects are its subclasses
  val anyRef = TClass(new ClassSymbol("AnyRef"))

  case class TClass(classSymbol: ClassSymbol) extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = {
      tpe match {
        case TNull => true
        case TClass(otherClassSymbol) =>
          classSymbol.isSubclassOf(otherClassSymbol.name)
        case _ => false
      }
    }

    def leastCommonParent(tpe: Type): Type = {
      if (tpe.isSubTypeOf(this))
        this
      else
        classSymbol.parent match {
          case Some(parentSymbol) => parentSymbol.getType.asInstanceOf[TClass].leastCommonParent(tpe)
          case None => anyRef
        }
    }

    override def toString: String = classSymbol.name

    override val compilerType: String = "L" + classSymbol.name + ";"
  }

  case object TError extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = true

    override def toString: String = ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>[error]<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"

    override val compilerType: String = "Error" // Shouldn't ever be used
  }

  case object TUntyped extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = false

    override def toString: String = ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>[untyped]<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"

    override val compilerType: String = "Untyped" // Shouldn't ever be used
  }

  case object TInt extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TInt => true
      case _ => false
    }

    override def toString: String = "Int"

    override val compilerType: String = "I"
  }

  case object TBoolean extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TBoolean => true
      case _ => false
    }

    override def toString: String = "Boolean"

    override val compilerType: String = "Z"
  }

  case object TString extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TString => true
      case _ => false
    }

    override def toString: String = "String"

    override val compilerType: String = "Ljava/lang/String;"
  }

  case object TUnit extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TUnit => true
      case _ => false
    }

    override def toString: String = "Unit"

    override val compilerType: String = "V"
  }

  case object TNull extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TNull => true
      case TClass(_) => true
      case _ => false
    }

    override def toString: String = "Null"

    override val compilerType: String = "L"
  }
}
