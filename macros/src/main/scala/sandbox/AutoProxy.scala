package sandbox

import scala.language.experimental.macros

import scala.reflect.macros.whitebox.Context

class proxy extends scala.annotation.StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro DelegatingMacro.impl
}

object DelegatingMacro {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    def showInvalidUsageError() {
      c.error(c.enclosingPosition, "This annotation can only be used on params, vals, vars or methods")
    }
        
    val typeDiscovery = TypeDiscovery[c.type](c)
    import typeDiscovery._
    
    /**
     * @param src: Symbol of the method (or accessor) being delegated
     * @param tgtClass: The class in which the delegate is being created
     * @param proxy: The member of tgtClass that we're delegating to
     */
    def mkDefDelegate(methodSym: MethodSymbol, proxy: ValOrDefDef): c.Tree = {
      
      val vparamss = methodSym.paramss.map(_.map {
        paramSymbol => ValDef(
          Modifiers(Flag.PARAM, tpnme.EMPTY, List()),
          paramSymbol.name.toTermName,
          TypeTree(paramSymbol.typeSignature),
          EmptyTree)
      })      
      
      val delegateInvocation = Apply(
        Select(Ident(proxy.name), methodSym.name),
        methodSym.paramss.flatMap(_.map(param => Ident(param.name)))) // TODO - multi params list

      //TODO: Flag as stable if methodSym and proxy are both also stable
      DefDef(Modifiers(),
        methodSym.name.toTermName,
        List(), // TODO - type parameters
        vparamss,
        TypeTree(methodSym.returnType),
        delegateInvocation)
    }
    
    
    def addDelegateMethods(valDef: ValOrDefDef, addToClass: ClassDef) = {
      def allMethodsInDelegate = computeType(valDef.tpt).declarations

      val ClassDef(mods, name, tparams, Template(parents, self, body)) = addToClass

      // TODO better filtering - allow overriding
      val existingMethods: Set[TermName] = body.flatMap(tree => tree match {
        case DefDef(_, n, _, _, _, _) => Some(n)
        case _ => None
      })(collection.breakOut)
      
      val methodsToAdd: Iterable[Symbol] = allMethodsInDelegate.filter(method => !existingMethods.contains(method.name.toTermName))

      val newMethods: Iterable[c.Tree] = for {
        methodToAdd <- methodsToAdd
      } yield {
        mkDefDelegate(methodToAdd.asMethod, valDef)
      }

      ClassDef(mods, name, tparams, Template(parents, self, body ++ newMethods))
    }
    
    val inputs = annottees.map(_.tree).toList
    
    val (_, expandees) = inputs match {
      case (param: ValOrDefDef) :: (enclosing: ClassDef) :: rest =>
        //We're a param, and the ClassDef is supplied
        val newEnclosing = addDelegateMethods(param, enclosing)
        (param, newEnclosing :: rest)

      case (param: ValOrDefDef) :: Nil =>
        //We're a member, and must work out who owns us
        println("param = " + param)
        showInvalidUsageError()
        (EmptyTree, inputs)
        
      case _ =>
        showInvalidUsageError()
        (EmptyTree, inputs)
    }
    val outputs = expandees
    c.Expr[Any](Block(outputs, Literal(Constant(()))))
  }
}