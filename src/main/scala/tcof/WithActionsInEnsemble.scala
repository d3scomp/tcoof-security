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

  def allow(objct: Component, action: String, subjects: Seq[Component]): Unit = allow(List(objct), action, subjects)
  def allow(objects: Seq[Component], action: String, subject: Component): Unit = allow(objects, action, List(subject))

  def allow(objects: Seq[Component], action: String, subjects: Seq[Component]): Unit = {
      // FIXME
  }


  def notifyOnce(subject: Component, notification: Notification): Unit = notifyOnce(List(subject), notification)

  def notifyOnce(subjects: Seq[Component], notification: Notification): Unit = {
    for (subject <- subjects) {
      subject.notify(notification)
    }
  }
}
