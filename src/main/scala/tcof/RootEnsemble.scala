package tcof

import tcof.InitStages.InitStages
import org.chocosolver.solver.Model

class RootEnsemble extends Ensemble {
  name("<root>")

  utility(_solverModel.sum(
    _ensembleGroups.values.map(_.sum(_.utility.getOrElse(_solverModel.IntegerInt(0))))
  ))

  override private[tcof] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)

    stage match {
      case InitStages.RulesCreation =>
        val sm = _solverModel
        utility match {
          case Some(sm.IntegerIntVar(utilityVar)) => _solverModel.setObjective(Model.MAXIMIZE, utilityVar)
          case _ =>
        }

        _solverModel.post(_buildEnsembleClause)
      case _ =>
    }
  }
}
