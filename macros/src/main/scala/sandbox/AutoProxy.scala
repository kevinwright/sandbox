package sandbox

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import scala.annotation.compileTimeOnly
import collection.breakOut
import LogUtils._

@compileTimeOnly("`@proxy` must be enclosed in a class annotated as `@delegating`")
class proxy extends scala.annotation.StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro DelegatingMacro.nakedProxyImpl
}

class delegating extends scala.annotation.StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro DelegatingMacro.impl
}

class proxytag extends scala.annotation.StaticAnnotation


object DelegatingMacro {

  def nakedProxyImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    c.abort(c.enclosingPosition,"`@proxy` must be enclosed in a class annotated as `@delegating`")
  }

  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    val theMacro = mkInstance(c)

    vprintln("annottees: " + annottees.mkString)

    val inputs = annottees.map(_.tree).toList

    val code = theMacro.process(inputs)

    vprintln(s"delegating macro transform expands to:\n ${code}")

    // Mark range positions for synthetic code as transparent to allow some wiggle room for overlapping ranges
    //for (t <- code) t.setPos(t.pos.makeTransparent)
    c.Expr[Any](Block(code, Literal(Constant(()))))
  }

  def mkInstance(c0: Context): DelegatingMacro { val c: c0.type } = {
    //import language.reflectiveCalls
    new DelegatingMacro {
      //type C = c0.type
      val c: c0.type = c0
    }
  }
}

trait MacroBase {
  //type C <: Context
  val c: Context
}

trait DelegatingMacro extends MacroBase with ClassCalculus {
  import c.universe._

  def posErr(s: String) = c.error(c.enclosingPosition,s)
  def posWarn(s: String) = c.warning(c.enclosingPosition,s)

  def showDelegateUsageError(): Unit = posErr("The @delegate annotation can only be used on classes or objects")
  def showProxyUsageError(): Unit = posErr("The @proxy annotation can only be used on params, vals, vars or methods")



  def treeOf[T <: ImplDef](impl: T): T =
    //typeCheckExpressionOfType(impl).asInstanceOf[T]
    c.typecheck(impl.duplicate, silent = true, withMacrosDisabled = true).asInstanceOf[T]

  def typeSymOf(impl: ImplDef): TypeSymbol =
    treeOf(impl).symbol.asType

  def injectMembers(templ: Template, newMembers: Seq[Tree], newInterfaces: List[Tree]): Template = {
    vprintln(s"templateParents = ${templ.parents}}")
    vprintln(s"injecting interfaces = ${newInterfaces}")
    Template(templ.parents ++ newInterfaces, templ.self, templ.body ++ newMembers)
  }


  def injectMembers[T <: ImplDef](impl: T, newMembers: Seq[Tree], newInterfaces: List[Tree]): T = impl match {
    case ClassDef(mods, name, tparams, templ) =>
      ClassDef(mods, name, tparams, injectMembers(templ, newMembers, newInterfaces)).asInstanceOf[T]

    case ModuleDef(mods, name, templ) =>
      ModuleDef(mods, name, injectMembers(templ, newMembers, newInterfaces)).asInstanceOf[T]
  }

  def hasProxyAnnotation(sym: Symbol): Boolean = {
//    println("annotations: " + sym.annotations.mkString(","))
    sym.annotations.exists(_.tree.toString == "new sandbox.proxytag()")
  }

  def proxyPivots(tpe: TypeSymbol): List[Symbol] = {
    val tpeInfo = tpe.info
    val ctorParams = tpeInfo.decls.collect{
      case ms: MethodSymbol if ms.name.toString == "<init>" => ms
    }.flatMap(_.paramLists.flatten)

    val candidates = ctorParams ++ tpeInfo.decls
//    val candidateTrees = candidates.map(_.tree)
    vprintln(s"candidate pivots for ${tpe.name} = $candidates")

    val pivots: List[Symbol] = candidates.flatMap{
      case s: Symbol if hasProxyAnnotation(s) => vprintln("identified pivot: " + showRaw(s)); List(s)
      case s: Symbol => vprintln("ignoring symbol " + showRaw(s) + " with " + s.annotations.map(_.tree)); Nil
      case x => vprintln("ignoring non-symbol " + x); Nil
    }(breakOut)

    vprintln(s"pivots for ${tpe.name} = $pivots")
    pivots
  }

  /**
   * @param origins A mapping from pivot symbols to symbols of provided methods
   * @return a List of delgating method trees
   */
  def mkDelegates(origins: Map[Symbol, Set[MethodSymbol]]): Seq[Tree] = {
    for {
      (pivot, methods) <- origins.toSeq
      method <- methods
      if !method.isConstructor
    } yield {
      val name = method.name.toTermName
      val paramss = method.paramLists
      val ret = method.returnType

      val delegateInvocation = {
        val argss = method.paramLists.map( _.map(param => Ident(param.name)) )
        q"${pivot.name.toTermName}.${method.name}(...$argss)"
      }

      val vparamss = method.paramLists.map(_.map {
        paramSymbol => ValDef(
          Modifiers(Flag.PARAM, typeNames.EMPTY, List()),
          paramSymbol.name.toTermName,
          TypeTree(paramSymbol.typeSignature),
          EmptyTree)
      })

      val delegate = q"""def $name(...${vparamss}): $ret = $delegateInvocation"""
      delegate
    }
  }

  object ProxyTaggingTransformer extends Transformer {
    override def transformModifiers(mods: Modifiers): Modifiers = {
      val Modifiers(flags, privateWithin, annotations) = mods
      val updatedannotations = annotations map { ann => ann match {
        case q"new $p()" if p.toString.endsWith("proxy") =>
          q"new sandbox.proxytag()"
        case x => x
      }}
      Modifiers(flags, privateWithin, updatedannotations)
    }
  }

  def tagProxyAnnotations[T <: Tree](tree: T): T = {
    ProxyTaggingTransformer.transform(tree).asInstanceOf[T]
  }

  def processClass(clazz0: ClassDef): Tree = {
    val clazz = tagProxyAnnotations(clazz0)
    val clazzSym = symOf(treeOf(clazz))
    val pivots = proxyPivots(clazzSym)

    val workSummary = summariseWork(clazzSym, pivots)
    import workSummary.{pivotProvidedMethods, pivotProvidedInterfaceTrees}

    pivotProvidedMethods foreach { case (pivot,methods) =>
      vprintln(s"Provided Methods for ${pivot.name} = ${methods.mkString}")
    }

//    pivotProvidedInterfaces foreach { case (pivot,interfaces) =>
//      vprintln(s"Provided interfaces for ${pivot.name} = ${interfaces.mkString}")
//    }

    if(c.hasErrors) clazz
    else injectMembers(clazz.duplicate.asInstanceOf[ClassDef], mkDelegates(pivotProvidedMethods), pivotProvidedInterfaceTrees)
  }

  def processModule(mod0: ModuleDef): ModuleDef = {
    val mod = tagProxyAnnotations(mod0)
    val modSym = symOf(treeOf(mod))
    val modClassSym = modSym.moduleClass.asClass
    val pivots = proxyPivots(modClassSym)

    val workSummary = summariseWork(modClassSym, pivots)
    import workSummary.{pivotProvidedMethods, pivotProvidedInterfaceTrees}

    pivotProvidedMethods foreach { case (pivot,methods) =>
      vprintln(s"Provided Methods for ${pivot.name} = ${methods.mkString}")
    }

//    pivotProvidedInterfaces foreach { case (pivot,interfaces) =>
//      vprintln(s"Provided interfaces for ${pivot.name} = ${interfaces.mkString}")
//    }

    val newClassDef = injectMembers(mod.duplicate.asInstanceOf[ModuleDef], mkDelegates(pivotProvidedMethods), pivotProvidedInterfaceTrees)
    newClassDef
  }

  def process(inputs: List[Tree]): List[Tree] = inputs match {
    // Workaround here for https://github.com/scalamacros/paradise/issues/50 when the class lacks a companion
    case (clazz: ClassDef) :: Nil       => processClass(clazz) :: q"object ${clazz.name.toTermName}" :: Nil
    case (clazz: ClassDef) :: rest      => processClass(clazz) :: rest
    case (singleton: ModuleDef) :: rest => processModule(singleton) :: rest
    case _                              => showDelegateUsageError(); inputs
  }
}


