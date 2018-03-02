package tcof

import tcof.InitStages.InitStages
import tcof.Utils._

import scala.collection.mutable

trait WithEnsembleGroups extends Initializable {
  this: WithConfig =>

  /** A set of all potential ensembles */
  private[tcof] val _ensembleGroups = mutable.Map.empty[String, EnsembleGroup[Ensemble]]

  def ensembles[EnsembleType <: Ensemble](ensFirst: EnsembleType, ensRest: EnsembleType*): EnsembleGroup[EnsembleType] = ensembles(randomName, ensRest.+:(ensFirst))

  def ensembles[EnsembleType <: Ensemble](ens: Iterable[EnsembleType]): EnsembleGroup[EnsembleType] = ensembles(randomName, ens)

  def ensembles[EnsembleType <: Ensemble](name: String, ensFirst: EnsembleType, ensRest: EnsembleType*): EnsembleGroup[EnsembleType] = ensembles(name, ensRest.+:(ensFirst))

  def ensembles[EnsembleType <: Ensemble](name: String, ens: Iterable[EnsembleType]): EnsembleGroup[EnsembleType] = {
    val group = new EnsembleGroup(name, new EnsembleGroupMembers(ens))
    _ensembleGroups += name -> group
    group
  }

  private[tcof] def _buildEnsembleGroupClause: Logical = _solverModel.and(_ensembleGroups.values.map(_.buildMembershipClause))

  override private[tcof] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)
    _ensembleGroups.values.foreach(_._init(stage, config))
  }
}
