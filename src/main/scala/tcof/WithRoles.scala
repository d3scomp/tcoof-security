package tcof

import tcof.InitStages.InitStages
import tcof.Utils._

import scala.collection.mutable

trait WithRoles extends Initializable with CommonImplicits {
  this: WithConfig =>

  private[tcof] val _roles: mutable.Map[String, Role[Component]] = mutable.Map.empty[String, Role[Component]]

  def oneOf[ComponentType <: Component](items: RoleMembers[ComponentType]): Role[ComponentType] =
    _addRole(randomName, items, cardinality => cardinality === 1)

  def _addRole[ComponentType <: Component](name: String, items: RoleMembers[ComponentType], cardinalityConstraints: Integer => Logical): Role[ComponentType] = {
    val role = new Role[ComponentType](name, this, items, cardinalityConstraints)
    _roles += name -> role
    role
  }

  override private[tcof] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)
    _roles.values.foreach(_._init(stage, config))
  }
}
