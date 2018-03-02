package tcof

import scala.reflect.ClassTag

/**
  * Collection of members that stems from another parent role. A member in this collection can be selected only if it is selected in the parent role.
  */
class RoleMembersCond[+ComponentType <: Component](val linkedMembers: Iterable[LinkedMember[ComponentType]]) extends RoleMembers(linkedMembers.map(_.value)) {

  /** Creates members from existing parent without any filtering. */
  def this(parent: WithMembers[ComponentType]) = this(parent.allMembers.values.zipWithIndex.map{ case (member, idx) => LinkedMember(member, parent, idx) })

  private[tcof] override def mapChildToParent(membersContainer: WithMembers[Component]): Unit = {
    val members = linkedMembers.zipWithIndex
    for ((member, idx) <- members) {
      _solverModel.ifThen(_solverModel.member(idx, membersContainer.allMembersVar), _solverModel.member(member.indexInParent, member.parent.allMembersVar))
    }
  }

  def selectCond[RoleType <: Component : ClassTag]: RoleMembersCond[RoleType] =
    new RoleMembersCond(linkedMembers.collect{ case member@LinkedMember(value: RoleType, parent, indexInParent) => LinkedMember(value.asInstanceOf[RoleType], parent, indexInParent) })

  def ++[B >: ComponentType <: Component](other: RoleMembersCond[B]): RoleMembersCond[B] = {
    val members = linkedMembers ++ other.linkedMembers
    new RoleMembersCond(members)
  }

}
