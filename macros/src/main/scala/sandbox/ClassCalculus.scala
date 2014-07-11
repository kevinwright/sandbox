package sandbox

import scala.collection.breakOut

trait ClassCalculus extends MacroBase {
  import c.universe._

  def symOf(x: ClassDef) = x.symbol.asClass
  def symOf(x: ModuleDef) = x.symbol.asModule
  def symOf(x: DefDef) = x.symbol.asMethod


  def methodsOn(s: Symbol): Set[MethodSymbol] = s.info.members.collect{
    case ms: MethodSymbol => ms
  }(breakOut)

  def summariseWork(clazzSym: ClassSymbol, pivots: List[Symbol]): WorkSummary =
    new WorkSummaryImpl(clazzSym, pivots)


  private class WorkSummaryImpl(val clazzSym: ClassSymbol, val pivots: List[Symbol]) extends WorkSummary {
    val clazzInfo = clazzSym.info

    val existingMethods: Set[MethodSymbol] = clazzInfo.members.collect{
      case ms: MethodSymbol => ms
    }(breakOut)

    val existingInterfaces = clazzInfo.baseClasses.toSet

    val (existingAbstractMethods, existingConcreteMethods) = existingMethods.partition(_.isAbstract)
    val existingConcreteMethodSigs = existingConcreteMethods map {_.typeSignature}

    val pivotProvidedMethods: Map[Symbol, Set[MethodSymbol]] = pivots.map{ p =>
      p -> (methodsOn(p) filter {m => !existingConcreteMethodSigs.exists(_ =:= m.typeSignature)})
    }(breakOut)

    {
      val flatProvidedMethods: Set[MethodSymbol] = pivotProvidedMethods.flatMap(_._2)(breakOut)
      val methodsToProviders: Map[MethodSymbol, Set[Symbol]] = flatProvidedMethods.map{ m =>
        val providers: Set[Symbol] = pivotProvidedMethods.collect{case (k,v) if v contains m => k}(breakOut)
        m -> providers
      }(breakOut)
      val dupes = methodsToProviders.filter(_._2.size > 1)
      //println("dupes: " + dupes)
      for((method, pivots) <- dupes) {
        c.error(c.enclosingPosition, s"ambiguous proxy, the method '${method.name}${method.typeSignature}' is provided by ${pivots.mkString("'", "' and '", "'")}")
      }
    }

    val pivotProvidedInterfaces: Map[Symbol, List[Symbol]] = pivots.map{ p =>
      //println(s"pivot base classes for ${p.name} = ${p.info.baseClasses}")
      //println(s"existing base classes for $clazzSym = $existingInterfaces")
      //tail, because we don't want to include the base type itself
      p -> (p.info.baseClasses filterNot existingInterfaces.contains filterNot (_.toString == p.toString))
    }(breakOut)

    private def stringDistinct[T](xs: List[T]): List[T] = {
      val b = List.newBuilder[T]
      val seen = collection.mutable.HashSet[String]()
      for (x <- xs) {
        if (!seen(x.toString)) {
          b += x
          seen += x.toString
        }
      }
      b.result()
    }

    val pivotProvidedInterfaceTrees: List[Tree] = stringDistinct(
      for {
        _ <- List(1) // force a List monad
        (_, syms) <- pivotProvidedInterfaces
        sym <- syms
      } yield {
        val symStr = sym.fullName
        //println("sym = " + symStr)
        val symPath = symStr.split('.').toList
        @annotation.tailrec def mkTree(path: List[String], acc: Tree = EmptyTree): Tree = path match {
          case x :: Nil => Select(acc, TypeName(x))
          case h :: t if acc == EmptyTree => mkTree(t, Ident(TermName(h)))
          case h :: t => mkTree(t, Select(acc, TermName(h)))
        }
        val symTree = mkTree(symPath)
        //println(s"$symTree")
        symTree
      }
    )

  }

  trait WorkSummary {
    def clazzSym: ClassSymbol
    def clazzInfo: Type
    def pivots: List[Symbol]

    def existingMethods: Set[MethodSymbol]
    def existingAbstractMethods: Set[MethodSymbol]
    def existingConcreteMethods: Set[MethodSymbol]

    def existingInterfaces: Set[Symbol]

    def pivotProvidedMethods: Map[Symbol, Set[MethodSymbol]]

    def pivotProvidedInterfaces: Map[Symbol, List[Symbol]]
    def pivotProvidedInterfaceTrees: List[Tree]
  }

}
