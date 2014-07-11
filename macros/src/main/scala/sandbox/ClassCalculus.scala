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


    val pivotProvidedInterfaces: Map[Symbol, List[Symbol]] = pivots.map{ p =>
      println(s"pivot base classes for ${p.name} = ${p.info.baseClasses}")
      println(s"existing base classes for $clazzSym = $existingInterfaces")
      //tail, because we don't want to include the base type itself
      p -> (p.info.baseClasses filterNot existingInterfaces.contains filterNot (_.toString == p.toString))
    }(breakOut)

    println(s"interfaces to inject = $pivotProvidedInterfaces")


    val pivotProvidedInterfaceTrees: List[Tree] = (
      for {
        _ <- List(1) // force a List monad
        (_, syms) <- pivotProvidedInterfaces
        sym <- syms
      } yield {
        val symStr = sym.fullName
        println("sym = " + symStr)
        val symPath = symStr.split('.').toList
        @annotation.tailrec def mkTree(path: List[String], acc: Tree = EmptyTree): Tree = path match {
          case x :: Nil => Select(acc, TypeName(x))
          case h :: t if acc == EmptyTree => mkTree(t, Ident(TermName(h)))
          case h :: t => mkTree(t, Select(acc, TermName(h)))
        }
        val symTree = mkTree(symPath)
        println(s"$symTree")
        symTree
        //q"${TermName(symStr)}"
      }
    ).distinct

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
