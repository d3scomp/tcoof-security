package tcof

import tcof.InitStages.InitStages

class StateSetEquiv[+StateType <: State](private[tcof] val allMembers: StateSetMembersEquiv[StateType]) extends WithMembers[StateType] with Initializable {

  override private[tcof] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)

    stage match {
      case InitStages.RulesCreation =>
        val members = allMembers.linkedMembers.zipWithIndex
        for ((member, idx) <- members) {

          _solverModel.ifOnlyIf(_solverModel.member(idx, allMembersVar), _solverModel.member(member.indexInParent, member.parent.allMembersVar))
        }

      case _ =>
    }
  }
}
