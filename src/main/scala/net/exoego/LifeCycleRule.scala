package net.exoego

trait LifeCycleRule {
  def apply(isActive: Boolean, livingNeighbours: Int): Boolean
}

case class NeighbourCount(birthWhen: Set[Int], surviveWhen: Set[Int]) extends LifeCycleRule {
  override def apply(isAlive: Boolean, neighbours: Int): Boolean = {
    if (isAlive) {
      surviveWhen.contains(neighbours)
    } else {
      birthWhen.contains(neighbours)
    }
  }
}

object LifeCycleRule {
  final val B3_S23 = NeighbourCount(birthWhen = Set(3), surviveWhen = Set(2, 3))
  final val Conway = B3_S23

  final val B36_S23  = NeighbourCount(birthWhen = Set(3, 6), surviveWhen = Set(2, 3))
  final val HighLife = B36_S23

  final val B34_S34 = NeighbourCount(birthWhen = Set(3, 4), surviveWhen = Set(3, 4))
  final val B2_S2   = NeighbourCount(birthWhen = Set(2), surviveWhen = Set(2))
}
