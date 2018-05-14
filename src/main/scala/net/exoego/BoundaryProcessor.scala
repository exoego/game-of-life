package net.exoego

sealed trait BoundaryProcessor {
  def apply(current: Int, rangeMax: Int): Seq[Int]
}

object BoundaryProcessor {

  case object Bounded extends BoundaryProcessor {
    override def apply(current: Int, rangeMax: Int): Seq[Int] = {
      val limit = rangeMax - 1
      current match {
        case 0       => Array(current, current + 1)
        case `limit` => Array(current - 1, current)
        case _       => Array(current - 1, current, current + 1)
      }
    }
  }

  case object Loop extends BoundaryProcessor {
    override def apply(current: Int, rangeMax: Int): Seq[Int] = {
      val limit = rangeMax - 1
      current match {
        case 0       => Array(rangeMax - 1, current, current + 1)
        case `limit` => Array(current - 1, current, 0)
        case _       => Array(current - 1, current, current + 1)
      }
    }
  }
}
