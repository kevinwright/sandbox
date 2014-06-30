package sandbox.proxytests

import sandbox.{delegating, proxy}

trait Bippy {
  def bippy(i : Int) : String
}

object SimpleBippy extends Bippy {
  def bippy(i: Int) = i.toString
}

object DoublingBippy extends Bippy {
  def bippy(i: Int) = (i*2).toString
}

@delegating
class BippyValParamWrapper(@proxy val dg : Bippy) {
  def one(s: String) = s
  def two(i: Int) = i
  def three[T](x: T) = x
  def dgtoo = dg
}

//class BippyVarParamWrapper(@proxy var dg : Bippy)

//class BippyValWrapper {
//  @proxy val dg = SimpleBippy
//}

//class BippyVarWrapper {
//  @proxy var dg = SimpleBippy
//}
