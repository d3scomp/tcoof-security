package tcof

class StateSetMembers[+StateType <: State](values: Iterable[StateType]) extends Members(values)
