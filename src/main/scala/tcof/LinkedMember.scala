package tcof

case class LinkedMember[+MemberType](value: MemberType, parent: WithMembers[_], indexInParent: Int)
