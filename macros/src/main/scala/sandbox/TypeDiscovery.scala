package sandbox

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

trait TypeDiscovery {
  val ctx: Context
  import ctx.universe._

  // Using a technique adapted from MacWire ...
  def typeCheckExpressionOfType(typeTree: Tree): Type = {
    val someValueOfGivenType @ Block(_,_) = q"def x[T](): T = throw new Exception; x[$typeTree]()"
    val someValueOfGivenTypeChecked = ctx.typecheck(someValueOfGivenType)
    someValueOfGivenTypeChecked.tpe
  }

  def computeType(tpt: Tree): Type = {

    Option(tpt.tpe) getOrElse {
      //typecheck will return EmptyTree on failure
      val calculatedType = ctx.typecheck(tpt.duplicate, silent = true, withMacrosDisabled = true).tpe
      //theory: the double-check is just in case it completed typing in parallel
      val result = Option(tpt.tpe) getOrElse calculatedType
  
      if (result == NoType)
        typeCheckExpressionOfType(tpt)
      else
        result
    }
  }
}

