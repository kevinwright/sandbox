package sandbox

import scala.language.experimental.macros

import scala.reflect.macros.whitebox.Context

class proxy extends scala.annotation.StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro DelegatingMacro.impl
}

object DelegatingMacro {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    object log {
      def err(msg: String): Unit = c.error(c.enclosingPosition, msg)
      def warn(msg: String): Unit = c.warning(c.enclosingPosition, msg)
      def info(msg: String): Unit = c.info(c.enclosingPosition, msg, force=true)
      def rawInfo(name: String, obj: Any): Unit = info(name + " = " + showRaw(obj))
    }

    def showInvalidUsageError(): Unit = log.err("This annotation can only be used on params, vals, vars or methods")


    val typeDiscovery = new { val ctx: c.type = c } with TypeDiscovery
    import typeDiscovery._
    
    /**
     * @param methodSym: Symbol of the method (or accessor) being delegated
     * @param proxy: The member of the delegating class that holds the object instance being delegated to
     */
    def mkDefDelegate(methodSym: MethodSymbol, proxy: ValOrDefDef): c.Tree = {

      //TODO: Quasiquotes (if not prevented by the need for flags)
      //This is only passed through as params to the synDef quasiquote, surely there must be a less verbose way?
      val vparamss = methodSym.paramss.map(_.map {
        paramSymbol => ValDef(
          Modifiers(Flag.PARAM, tpnme.EMPTY, List()),
          paramSymbol.name.toTermName,
          TypeTree(paramSymbol.typeSignature),
          EmptyTree)
      })



//      log.rawInfo("methodSym.typeParams", methodSym.typeParams)
//      log.rawInfo("methodSym.paramss", methodSym.paramss)
//      log.rawInfo("vparamss", vparamss)


      // TODO: multi params list
      val delegateInvocation = {
        val paramss = methodSym.paramss.flatMap( _.map(param => Ident(param.name)) )
        q"${proxy.name}.${methodSym.name}(..$paramss)"
      }

      //log.rawInfo("delegateInvocation", delegateInvocation)

      // TODO - type parameters
      //TODO: Flag as stable if methodSym and proxy are both also stable
      //Query: Has the STABLE flag vanished?  If so, then what's the alternative mechanism that we can hijack?
      //val dd2ret = tq"${methodSym.returnType}"
      val synDef @ DefDef(_,_,_,_,_,_) = {
        val name = methodSym.name.toTermName
        val ret = methodSym.returnType
        q"def $name(...$vparamss): $ret = $delegateInvocation"
      }
      synDef.symbol.isStatic
      log.rawInfo("synDef", synDef)
      synDef
    }

    def methodsIn(body: List[Tree]): Set[DefDef] =
      body.flatMap{ case dd : DefDef => Some(dd); case _ => None }(collection.breakOut)

    //TODO: Isolate logic that injects into a ClassDef
    //  If delegating via a non-param member then we won't have this surrounding
    //  class's AST injected into the macro and directly modifiable.
    //  Instead, we can perform an in-place expansion of one def to multiple defs,
    //  and warn the user of our inability to also expose proxied interfaces.
    def addDelegateMethods(pivot: ValOrDefDef, addToClass: ClassDef) = {

      val ClassDef(mods, name, tparams, Template(parents, self, body)) = addToClass

      val tcbody = c.typecheck(c.enclosingUnit.body.duplicate, withMacrosDisabled = true)
      log.rawInfo("body", tcbody)

//      val ctorBlock @ Block(_,_) = q"val x = new ${addToClass.name.toTermName}; x"
//      log.rawInfo("ctorBlock", ctorBlock)
//      val ctorType = ctx.typecheck(ctorBlock, silent = true, withMacrosDisabled = true).tpe
//      log.rawInfo("ctorType", ctorType)

      // TODO: better filtering - allow overriding
      //   So it's not enough to just check names, we must check the params as well
      //     full method sig is DefDef(mods: Modifiers, name: TermName, tparams: List[TypeDef], vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree): DefDef
      //   Query: Is it permitted to override on the return type here?
      //   Also consider asSeenFrom variants when proxying the same trait multi times with diff type params
      //   May need asSeenFrom logic to disambiguate where type params are involved
      //   For now, we WON'T allow dupes differing only by tparams, must check interaction with erasure first!
      //   Query: Should delegates override inherited members? Work out which default behaviour is most easily overridden
      //          Ideally we can subvert (then remove?) the `override` modifier, but does that work on params?
      //          Alternatively offer @proxy and @backupproxy
      val existingMethods: Set[TermName] = methodsIn(body) map (_.name)

      /*
      methodsIn(body) foreach { dd =>
        //ALL I WANT IS THE F*ING TYPE Of THE METHOD. Why must it be *so* hard to get something that ain't `Any`?
        log.rawInfo(dd.name.toString, dd)

        val tc = c.typecheck(q"{${dd.duplicate}}", silent = false, withMacrosDisabled = true)
        log.rawInfo("typechecked", tc.tpe)
        val expr = c.Expr(tc)
        log.rawInfo("expr type", expr.actualType)

        log.rawInfo("dd.rhs", dd.rhs)
        log.rawInfo("dd.tpt", dd.tpt)
        val tpe = computeType(dd.tpt)
        log.info(dd.name.toString + ".type = " + tpe)

        try {
          val blk = q"class X { ${dd.duplicate} }" // reify {c.Expr[Any](dd).splice} //q"$dd; dd _"
          val blktype = ctx.typecheck(blk)
          log.rawInfo("blk.tpe", blktype.tpe)
        } catch { case e: Throwable => log.warn(e.toString)}
      }
      */

      val methodsToAdd: Iterable[Symbol] = {
        log.rawInfo("pivot.tpt", pivot.tpt)
        def allMethodsInDelegate: Iterable[Symbol] = computeType(pivot.tpt).declarations
        allMethodsInDelegate.filter(method => !existingMethods(method.name.toTermName))
      }

      val newMethods: Iterable[c.Tree] = for {
        methodToAdd <- methodsToAdd
      } yield {
        mkDefDelegate(methodToAdd.asMethod, pivot)
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
        //We're a member, and must work out who owns us. Have to warn of reduced functionality
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