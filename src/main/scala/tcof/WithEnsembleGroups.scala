package tcof

import tcof.InitStages.InitStages
import tcof.Utils._

import scala.collection.mutable

trait WithEnsembleGroups extends Initializable {
  this: WithConfig =>

  def createIfPossible[EnsembleType <: Ensemble](ens: EnsembleType*): EnsembleGroup[EnsembleType] = {
    _addEnsembleGroup(randomName, ens, true)
  }

  /** A set of all potential ensembles */
  private[tcof] val _ensembleGroups = mutable.Map.empty[String, EnsembleGroup[Ensemble]]

/*
  FIXME
  def ensembles[EnsembleType <: Ensemble](ensFirst: EnsembleType, ensRest: EnsembleType*): EnsembleGroup[EnsembleType] = ensembles(randomName, ensRest.+:(ensFirst))

  def ensembles[EnsembleType <: Ensemble](ens: Iterable[EnsembleType]): EnsembleGroup[EnsembleType] = ensembles(randomName, ens)

  def ensembles[EnsembleType <: Ensemble](name: String, ensFirst: EnsembleType, ensRest: EnsembleType*): EnsembleGroup[EnsembleType] = ensembles(name, ensRest.+:(ensFirst))

  def ensembles[EnsembleType <: Ensemble](name: String, ens: Iterable[EnsembleType]): EnsembleGroup[EnsembleType] = _addEnsembleGroup(name, ens, false)
*/

  def _addEnsembleGroup[EnsembleType <: Ensemble](name: String, ens: Iterable[EnsembleType], createMemberIfCanExist: Boolean): EnsembleGroup[EnsembleType] = {
    val group = new EnsembleGroup(name, new EnsembleGroupMembers(ens), createMemberIfCanExist)
    _ensembleGroups += name -> group
    group
  }

  override private[tcof] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)
    _ensembleGroups.values.foreach(_._init(stage, config))
  }
}
