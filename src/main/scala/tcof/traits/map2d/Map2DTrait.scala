package tcof.traits.map2d

import tcof.traits.Trait


trait Map2DTrait[NodeStatusType] extends Trait {

  val map: Map2D[NodeStatusType] = new Map2D[NodeStatusType]

  override def init(): Unit = {
    super.init()
  }
}
