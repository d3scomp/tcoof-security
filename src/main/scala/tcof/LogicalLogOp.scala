package tcof

import org.chocosolver.solver.constraints.nary.cnf.LogOp

/** And/Or tree of clauses. This is used to represent clauses about membership of a component. */
private[tcof] case class LogicalLogOp(value: LogOp) extends LogicalWithILogic {
  protected type ValueType = LogOp

  override def unary_!(): Logical = LogicalLogOp(LogOp.nand(value))
}
