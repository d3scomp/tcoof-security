package tcof

import scala.collection.mutable

trait WithActionsInEnsemble extends WithActions {
  this: Ensemble =>

  private[tcof] var _actions = mutable.ListBuffer.empty[() => Unit]

  private[tcof] override def _executeActions(): Unit = {
    for (group <- _ensembleGroups.values) {
      group.selectedMembers.foreach(_._executeActions())
    }

    _actions.foreach(_())
  }

  def allow(subjects: Seq[Component], action: String, objct: Component): Unit = allow(subjects, action, List(objct))
  def allow(subject: Component, action: String, objects: Seq[Component]): Unit = allow(List(subject), action, objects)

  def allow(subjects: Seq[Component], action: String, objects: Seq[Component]): Unit = {
    _actions += (
      () => {
        for {
          objct <- objects
          subject <- subjects
        } {
          println(s"Allow ${subject} ${action} ${objct}")
        }
      }
    )
  }


  def notify(subjects: Seq[Component], notification: Notification): Unit = {
    _actions += (
      () => {
        for (subject <- subjects) {
          subject.notify(notification)
        }
      }
    )
  }
}
