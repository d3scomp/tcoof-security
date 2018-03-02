package tcof

import tcof.InitStages.InitStages
import org.chocosolver.solver.variables.SetVar

trait WithMembers[+MemberType] extends WithConfig with Initializable {

  private[tcof] def allMembers: Members[MemberType]

  private[tcof] var allMembersVar: SetVar = null

  override private[tcof] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)

    stage match {
      case InitStages.VarsCreation => allMembersVar = _solverModel.setVar(Array.empty[Int], 0 until allMembers.size toArray)
      case _ =>
    }
  }

  def cardinality: Integer = _solverModel.IntegerIntVar(allMembersVar.getCard)

  def contains(member: Any): Logical = some(x => LogicalBoolean(x == member))
  def containsOtherThan(member: Any): Logical = some(x => LogicalBoolean(x != member))
  def containsOnly(member: Any): Logical = all(x => LogicalBoolean(x == member))

  def sum(fun: MemberType => Integer): Integer = _solverModel.sumBasedOnMembership(allMembersVar, allMembers.values.map(fun))

  def all(fun: MemberType => Logical): Logical =
    _solverModel.forAllSelected(allMembers.values.map(fun), allMembersVar)

  def some(fun: MemberType => Logical): Logical =
    _solverModel.existsSelected(allMembers.values.map(fun), allMembersVar)

  def foreachBySelection(forSelected: MemberType => Unit, forNotSelected: MemberType => Unit): Unit = {
    val selection = _solverModel.solution.getSetVal(allMembersVar)
    for ((member, idx) <- allMembers.values.zipWithIndex) {
      if (selection.contains(idx))
        forSelected(member)
      else
        forNotSelected(member)
    }
  }

  def membersWithSelectionIndicator: Iterable[(Boolean, MemberType)] = {
    val selection = _solverModel.solution.getSetVal(allMembersVar)
    allMembers.values.zipWithIndex.map{case (member, idx) => (selection.contains(idx), member)}
  }

  def selectedMembers: Iterable[MemberType] = {
    val values = allMembers.values.toIndexedSeq
    for (idx <- _solverModel.solution.getSetVal(allMembersVar)) yield values(idx)
  }
}

