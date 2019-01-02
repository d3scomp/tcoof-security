package tcof.traits.map2d

import tcof.traits.Trait


trait Map2DTrait[NodeDataType] extends Trait {
  def map: Map2D[NodeDataType]

  override def init(): Unit = {
    super.init()
  }
}
