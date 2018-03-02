package tcof

class State(val id: Int, val name: String) {
  private[tcof] var parent: StateSet[_ <: State] = null
  private[tcof] var indexInParent: Int = _

  override def toString: String = s"$name"
}


