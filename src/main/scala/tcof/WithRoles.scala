package tcof

import tcof.InitStages.InitStages
import tcof.Utils._

import scala.collection.mutable

trait WithRoles extends Initializable {
  this: WithConfig =>

  private[tcof] val _roles: mutable.Map[String, Role[Component]] = mutable.Map.empty[String, Role[Component]]

  def role[ComponentType <: Component](items: RoleMembers[ComponentType]): Role[ComponentType] = role(randomName, items)
  def role[ComponentType <: Component](name: String, items: RoleMembers[ComponentType]): Role[ComponentType] = {
    val role = new Role[ComponentType](name, this, items)
    _roles += name -> role
    role
  }

  override private[tcof] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)
    _roles.values.foreach(_._init(stage, config))
  }
}
