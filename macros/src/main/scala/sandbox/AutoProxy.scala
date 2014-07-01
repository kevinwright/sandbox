package sandbox

import scala.language.experimental.macros

import scala.reflect.macros.whitebox.Context

import collection.breakOut

class proxy extends scala.annotation.StaticAnnotation

class delegating extends scala.annotation.StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro DelegatingMacro.impl
}

object DelegatingMacro {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    println("in the macro!")
    object log {
      def err(msg: String): Unit = c.error(c.enclosingPosition, msg)
      def warn(msg: String): Unit = c.warning(c.enclosingPosition, msg)
      def info(msg: String): Unit = c.info(c.enclosingPosition, msg, force=true)
      def rawInfo(name: String, obj: Any): Unit = info(name + " = " + showRaw(obj))
    }

    println("annottees: " + annottees.mkString)

    def showDelegateUsageError(): Unit = log.err("The @delegate annotation can only be used on classes or objects")//params, vals, vars or methods")


    val typeDiscovery = new { val ctx: c.type = c } with TypeDiscovery
    import typeDiscovery._
    
    /**
     * @param methodSym: Symbol of the method (or accessor) being delegated
     * @param proxy: The member of the delegating class that holds the object instance being delegated to
     */
    def mkDefDelegate(methodSym: MethodSymbol, proxy: ValOrDefDef): c.Tree = {

      //TODO: Quasiquotes (if not prevented by the need for flags)
      //This is only passed through as params to the synDef quasiquote, surely there must be a less verbose way?
      val vparamss = methodSym.paramLists.map(_.map {
        paramSymbol => ValDef(
          Modifiers(Flag.PARAM, typeNames.EMPTY, List()),
          paramSymbol.name.toTermName,
          TypeTree(paramSymbol.typeSignature),
          EmptyTree)
      })



//      log.rawInfo("methodSym.typeParams", methodSym.typeParams)
//      log.rawInfo("methodSym.paramss", methodSym.paramss)
//      log.rawInfo("vparamss", vparamss)


      // TODO: multi params list
      val delegateInvocation = {
        val paramss = methodSym.paramLists.flatMap( _.map(param => Ident(param.name)) )
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
      body.flatMap{ case dd : DefDef => List(dd); case _ => Nil }(collection.breakOut)

    def treeOf[T <: ImplDef](impl: T): T = {
      val out = ctx.typecheck(impl.duplicate, silent = true, withMacrosDisabled = true)
      out.asInstanceOf[T]
    }

    def symOf(impl: ImplDef): TypeSymbol =
      treeOf(impl).symbol.asType

//    def classSymOf(clazz: ClassDef): ClassSymbol =
//      treeOf(clazz).symbol.asClass
//
//    def moduleSymOf(mod: ModuleDef): ModuleSymbol =
//      treeOf(mod).symbol.asModule

    //TODO: Isolate logic that injects into a ClassDef
    //  If delegating via a non-param member then we won't have this surrounding
    //  class's AST injected into the macro and directly modifiable.
    //  Instead, we can perform an in-place expansion of one def to multiple defs,
    //  and warn the user of our inability to also expose proxied interfaces.
    def addDelegateMethods(pivot: ValOrDefDef, addToClass: ClassDef) = {

      val classSym = symOf(addToClass)
      val classType = classSym.info

      log.rawInfo("classSym", classSym)

      val ClassDef(mods, name, tparams, Template(parents, self, body)) = addToClass

//      val tcbody = c.typecheck(c.enclosingUnit.body.duplicate, withMacrosDisabled = true)
//      log.rawInfo("body", tcbody)

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

//      classSym

/*
      methodsIn(body) foreach { dd =>
        val name = dd.name.toString
        //ALL I WANT IS THE F*ING TYPE Of THE METHOD. Why must it be *so* hard to get something that ain't `Any`?
        log.rawInfo(name, dd)

//        val tc = c.typecheck(q"{${dd.duplicate}}", silent = false, withMacrosDisabled = true)
//        log.rawInfo("typechecked", tc.tpe)
//        val expr = c.Expr(tc)
//        log.rawInfo("expr type", expr.actualType)

//        log.rawInfo(name + ".rhs", dd.rhs)
//        log.rawInfo(name + ".tpt", dd.tpt)
//        val tpe = computeType(dd.tpt)
//        log.info(name + ".tpe = " + tpe)

        try {
          //val blk = q"${dd.duplicate}" // reify {c.Expr[Any](dd).splice} //q"$dd; dd _"
          val typechecked = c.typecheck(dd)
          //val typechecked = ctx.typecheck(q"${dd.duplicate}")
          //log.rawInfo(name + ".typechecked", typechecked)
          if(dd.name.toString == "<init>") { log.info("=CONSTRUCTOR=")}
          log.info(name + ".typechecked.symbol = " + typechecked.symbol)
          //log.rawInfo(name + ".typechecked.symbol [raw]", typechecked.symbol)
          log.info(name + ".typechecked.symbol.info = " + typechecked.symbol.info)
          log.rawInfo(name + ".typechecked.symbol.info [raw]", typechecked.symbol.info)
          //log.rawInfo(name + ".typechecked.tpe", typechecked.tpe)
        } catch { case e: Throwable => log.warn(e.toString)}
      }
*/

      val methodsToAdd: Iterable[Symbol] = {
        log.rawInfo("pivot.tpt", pivot.tpt)
        def allMethodsInDelegate: Iterable[Symbol] = computeType(pivot.tpt).decls
        allMethodsInDelegate.filter(method => !existingMethods(method.name.toTermName))
      }

      val newMethods: Iterable[c.Tree] = for {
        methodToAdd <- methodsToAdd
      } yield {
        mkDefDelegate(methodToAdd.asMethod, pivot)
      }

      ClassDef(mods, name, tparams, Template(parents, self, body ++ newMethods))
    }


//    def proxyPivots(impl: ImplDef): List[Tree] = {
//
//      val ProxyAnnotationRegex="""new .*proxy\(\)""".r
//
//      def hasProxyAnnotation(m: MemberDef): Boolean = m.mods.annotations.exists{
//        case x if ProxyAnnotationRegex.findFirstIn(x.toString).isDefined => true
//        case x =>
//          log.info(s"bad annotation: [${x.toString}]")
//          false
//      }
//
//      impl.impl.body collect {
////        case m: MemberDef if hasProxyAnnotation(m) => m
//        case m: MemberDef =>
//          log.info("mod annotations on [" + m.toString + "] = " + m.mods.annotations.mkString)
//          log.info("sym annotations on [" + m.toString + "] = " + m.symbol.annotations.mkString)
//          m
//
//        case m =>
//          log.info("sym annotations on [" + m.toString + "] = " + m.symbol.annotations.mkString)
//          m
//      }
//    }

    def injectMembers(clazz: ClassDef, newMembers: Seq[Tree]): ClassDef = {
      val ClassDef(mods, name, tparams, Template(parents, self, body)) = clazz
      ClassDef(mods, name, tparams, Template(parents, self, body ++ newMembers))
    }

    def proxyPivots(sym: TypeSymbol): Seq[Symbol] = {

      def hasProxyAnnotation(sym: Symbol): Boolean =
        sym.annotations.exists(_.toString == classOf[proxy].getName)

      val ctorParams = sym.info.decls collect {
        case ms: MethodSymbol if ms.name.toString == "<init>" => ms
      } flatMap(_.paramLists.flatten)

      val candidates = ctorParams ++ sym.info.decls

      candidates.collect{
        case s: Symbol if hasProxyAnnotation(s) => s
      }.toSeq
    }

    def processClass(clazz: ClassDef): ClassDef = {
      val tcClazz = treeOf(clazz)
      val sym = tcClazz.symbol.asType
      val pivots = proxyPivots(sym)

      val existingMethods = sym.info.members.collect{
        case ms: MethodSymbol => ms
      }.toSet

      val existingInterfaces = sym.info.baseClasses.toSet

      val (existingAbstractMethods, existingConcreteMethods) = existingMethods.partition(_.isAbstract)

      val proxyProvidedMethods: Map[Symbol, Set[MethodSymbol]] = pivots.map{ p =>
        p -> p.info.members.collect{
          case ms: MethodSymbol if !existingConcreteMethods.contains(ms) => ms
        }.toSet
      }(breakOut)

      val proxyProvidedInterfaces: Map[Symbol, Set[Symbol]] = pivots.map{ p =>
        p -> p.info.baseClasses.filter{ !existingInterfaces.contains(_) }.toSet
      }(breakOut)

      proxyProvidedMethods foreach { case (pivot,methods) =>
        println(s"PPM for ${pivot.name} = ${methods.mkString}")
      }

      val delegates: Seq[Tree] = {
        for {
          (pivot, methods) <- proxyProvidedMethods.toSeq
          method <- methods
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
          println("delegate: " + delegate)
          delegate
        }
      }

      injectMembers(clazz, delegates)
    }

    def processModule(mod: ModuleDef): ModuleDef = {
      val sym = symOf(mod)
      mod
    }

    val inputs = annottees.map(_.tree).toList
    
    val (annottee, expandees) = inputs match {
      case (clazz: ClassDef) :: rest =>
        (EmptyTree, processClass(clazz) :: rest)

      case (singleton: ModuleDef) :: rest =>
        (EmptyTree, processModule(singleton) :: rest)

//      case (param: ValOrDefDef) :: (enclosing: ClassDef) :: rest =>
//        //We're a param, and the ClassDef is supplied
////        val newEnclosing = addDelegateMethods(param, enclosing)
////        (param, newEnclosing :: rest)
//        showDelegateUsageError()
//        (EmptyTree, inputs)
//
//      case (param: ValOrDefDef) :: Nil =>
//        //We're a member, and must work out who owns us. Have to warn of reduced functionality
//        showDelegateUsageError()
//        (EmptyTree, inputs)
        
      case _ =>
        showDelegateUsageError()
        (EmptyTree, inputs)
    }
    val outputs = expandees
    c.Expr[Any](Block(outputs, Literal(Constant(()))))
  }
}