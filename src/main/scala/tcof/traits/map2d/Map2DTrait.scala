package tcof.traits.map2d

import tcof.traits.Trait


trait Map2DTrait[NodeDataType] extends Trait {

  val map: Map2D[NodeDataType] = new Map2D[NodeDataType]

  override def init(): Unit = {
    super.init()
  }
}
