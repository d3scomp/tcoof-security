package tcof

class StateSetMembersEquiv[+StateType <: State](val linkedMembers: Iterable[LinkedMember[StateType]]) extends Members(linkedMembers.map(_.value))
