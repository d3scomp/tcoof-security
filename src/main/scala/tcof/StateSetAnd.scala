package tcof

import tcof.InitStages.InitStages

class StateSetAnd[+StateType <: State](stateId: Int, name: String, allMembers: StateSetMembers[StateType]) extends StateSet(stateId, name, allMembers) {
  override private[tcof] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)

    stage match {
      case InitStages.RulesCreation =>
        // The root state is StateSet. Thus we assume that a parent is present
        val membershipConstraint = (0 until allMembers.size).map(idx => _solverModel.member(idx, allMembersVar)).toArray
        _solverModel.ifThen(_solverModel.member(indexInParent, parent.allMembersVar), _solverModel.and(membershipConstraint: _*))

      case _ =>
    }
  }

}
