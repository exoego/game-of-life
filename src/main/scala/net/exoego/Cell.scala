package net.exoego

import scala.util.Random

sealed trait CellState

case object Dead extends CellState

case object Alive extends CellState

object Cell {
  private final val rand: Random              = new Random(java.security.SecureRandom.getInstanceStrong)
  private final val probabilityOfAliveAtStart = 15
}

case class Cell(private var state: CellState, private var lastState: CellState) {

  def currentState(): CellState = this.state

  import Cell._

  def toggle(): Unit = {
    if (wasActiveLast()) {
      this.die()
    } else {
      this.born()
    }
  }

  def mutate(): Unit = {
    if (rand.nextInt(100) > probabilityOfAliveAtStart) {
      this.die()
    } else {
      this.born()
    }
  }

  def wasActiveLast(): Boolean = this.lastState == Alive

  def saveState(): Unit = {
    this.lastState = this.state
  }

  def nextState(neighbours: Int): Unit = {
    this.state match {
      case Alive if neighbours < 2 || neighbours > 3 => die()
      case Dead if neighbours == 3                   => born()
      case _                                         => // do nothing
    }
  }

  def die(): Unit = this.state = Dead

  def born(): Unit = this.state = Alive
}
