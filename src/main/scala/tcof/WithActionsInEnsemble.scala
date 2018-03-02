package tcof

trait WithActionsInEnsemble extends WithActions {
  this: Ensemble =>

  private[tcof] override def _executePreActions(): Unit = {
    for (group <- _ensembleGroups.values) {
      group.allMembers.values.foreach(_._executePreActions())
    }

    _preActions.foreach(_())
  }

  private[tcof] override def _executeActions(): Unit = {
    for (group <- _ensembleGroups.values) {
      group.selectedMembers.foreach(_._executeActions())
    }

    _actions.foreach(_())
  }
}
