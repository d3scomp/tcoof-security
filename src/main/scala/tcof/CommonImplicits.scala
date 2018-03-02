package tcof

trait CommonImplicits {
  this: WithConfig =>

  implicit class WithMembersIterable[MemberType](memberGroups: Iterable[WithMembers[MemberType]]) {
    def allDisjoint: Logical =
      if (memberGroups.isEmpty)
        LogicalBoolean(true)
      else
      LogicalBoolVar(_solverModel.allDisjoint(memberGroups.map(_.allMembersVar).toArray : _*).reify())
  }

  implicit def booleanToLogical(x: Boolean): LogicalBoolean = LogicalBoolean(x)
  implicit def intToInteger(value: Int): Integer = _solverModel.IntegerInt(value)

}
