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

sealed trait BoundaryProcessor {
  def apply(current: Int, rangeMax: Int): Seq[Int]
}

case object Loop extends BoundaryProcessor {
  override def apply(current: Int, rangeMax: Int): Seq[Int] = {
    val limit = rangeMax - 1
    current match {
      case 0       => Array(current, current + 1)
      case `limit` => Array(current - 1, current)
      case _       => Array(current - 1, current, current + 1)
    }
  }
}

case object Bounded extends BoundaryProcessor {
  override def apply(current: Int, rangeMax: Int): Seq[Int] = {
    val limit = rangeMax - 1
    current match {
      case 0       => Array(rangeMax - 1, current, current + 1)
      case `limit` => Array(current - 1, current, 0)
      case _       => Array(current - 1, current, current + 1)
    }
  }
}
