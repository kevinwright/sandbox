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

  def summariseWork(clazzInfo: Type, pivots: List[Symbol]): WorkSummary =
    new WorkSummaryImpl(clazzInfo, pivots)


  private class WorkSummaryImpl(val clazzInfo: Type, val pivots: List[Symbol]) extends WorkSummary {
    val existingMethods: Set[MethodSymbol] = clazzInfo.members.collect{
      case ms: MethodSymbol => ms
    }(breakOut)

    val existingInterfaces = clazzInfo.baseClasses.toSet

    val (existingAbstractMethods, existingConcreteMethods) = existingMethods.partition(_.isAbstract)
    val existingConcreteMethodSigs = existingConcreteMethods map {_.typeSignature}

    val pivotProvidedMethods: Map[Symbol, Set[MethodSymbol]] = pivots.map{ p =>
      p -> (methodsOn(p) filter {m => !existingConcreteMethodSigs.exists(_ =:= m.typeSignature)})
    }(breakOut)

    val pivotProvidedInterfaces: Map[Symbol, Set[Symbol]] = pivots.map{ p =>
      p -> (p.info.baseClasses.toSet -- existingInterfaces)
    }(breakOut)

  }

  trait WorkSummary {
    def clazzInfo: Type
    def pivots: List[Symbol]

    def existingMethods: Set[MethodSymbol]
    def existingAbstractMethods: Set[MethodSymbol]
    def existingConcreteMethods: Set[MethodSymbol]

    def existingInterfaces: Set[Symbol]

    def pivotProvidedMethods: Map[Symbol, Set[MethodSymbol]]

    def pivotProvidedInterfaces: Map[Symbol, Set[Symbol]]
  }

}
