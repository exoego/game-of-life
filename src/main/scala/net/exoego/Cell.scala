package net.exoego

sealed trait Cell {
  val isLive: Boolean
  val toggle: Cell
  def nextState(neighbours: Int): Cell
}

case object Dead extends Cell {
  def nextState(neighbours: Int): Cell = {
    neighbours match {
      case 3 => Live
      case _ => Dead
    }
  }

  override val toggle: Cell    = Live
  override val isLive: Boolean = false
}

case object Live extends Cell {
  def nextState(neighbours: Int): Cell = {
    neighbours match {
      case 2 | 3 => Live
      case _     => Dead
    }
  }
  override val toggle: Cell    = Dead
  override val isLive: Boolean = true
}
