package net.exoego

trait LifeCycleRule {
  def apply(isActive: Boolean, livingNeighbours: Int): Boolean
}

case object Rule23_3 extends LifeCycleRule {
  override def apply(isAlive: Boolean, neighbours: Int): Boolean = {
    (isAlive, neighbours) match {
      case (true, 2) => true
      case (_, 3)    => true
      case _         => false
    }
  }
}
