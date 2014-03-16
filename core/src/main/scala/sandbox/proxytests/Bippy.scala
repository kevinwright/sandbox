package sandbox.proxytests

import sandbox.proxy

trait Bippy {
  def bippy(i : Int) : String
}

object SimpleBippy extends Bippy {
  def bippy(i: Int) = i.toString
}

object DoublingBippy extends Bippy {
  def bippy(i: Int) = (i*2).toString
}

class BippyValParamWrapper(@proxy val dg : Bippy) {
  def one(s: String) = s
  def one(i: Int) = i
  def two[T](x: T) = x
  def dgtoo = dg
}
//class BippyVarParamWrapper(@proxy var dg : Bippy)

//class BippyValWrapper {
//  @proxy val dg = SimpleBippy
//}
//
//class BippyVarWrapper {
//  @proxy var dg = SimpleBippy
//}
