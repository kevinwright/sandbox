package sandbox

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

class TypeDiscovery[C <: Context](val c: C) {
  
  import c.universe._

  object Impl {
    // From MacWire ...
    def typeCheckExpressionOfType(typeTree: c.Tree): c.Type = {
      val someValueOfTypeString = reify {
        def x[T](): T = throw new Exception
        x[String]()
      }

      val Expr(Block(stats, Apply(TypeApply(someValueFun, _), someTypeArgs))) = someValueOfTypeString
  
      val someValueOfGivenType = Block(stats, Apply(TypeApply(someValueFun, List(typeTree)), someTypeArgs))
      val someValueOfGivenTypeChecked = c.typecheck(someValueOfGivenType)

      someValueOfGivenTypeChecked.tpe
    }

    def computeType(tpt: c.Tree): c.Type = {
      if (tpt.tpe != null) {
        tpt.tpe
      } else {
        val calculatedType = c.typecheck(tpt.duplicate, silent = true, withMacrosDisabled = true).tpe
        val result = if (tpt.tpe == null) calculatedType else tpt.tpe
  
        if (result == NoType) {
          typeCheckExpressionOfType(tpt)
        } else {
          result
        }
      }
    }
    // ... until here
  }
}

object TypeDiscovery {
  def apply[C <: Context](c: C)  = new TypeDiscovery[C](c).Impl
}