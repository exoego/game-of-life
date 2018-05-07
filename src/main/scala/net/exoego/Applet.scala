package net.exoego

import processing.core.PApplet
import processing.core.PApplet._
import processing.core.PConstants._

import scala.util.Random

class Applet extends PApplet {
  private final val COLOR_ALIVE = color(248, 221, 140)
  private final val COLOR_DEAD  = color(0)
  private final val COLOR_GRID  = color(48)

  private final val cellSize = 5

  sealed trait CellState

  case object Dead extends CellState

  case object Alive extends CellState

  object Cell {
    private final val rand: Random              = new Random(java.security.SecureRandom.getInstanceStrong)
    private final val probabilityOfAliveAtStart = 15
  }

  case class Cell(private var state: CellState, private var lastState: CellState) {
    import Cell._

    def toggle(): Unit = {
      if (wasActiveLast()) {
        this.die()
      } else {
        this.born()
      }
    }

    def color(): Int = {
      this.state match {
        case Dead  => COLOR_DEAD
        case Alive => COLOR_ALIVE
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

  private var cells: Array[Array[Cell]] = Array.empty

  private var paused: Boolean = false

  private def rows_ = width / cellSize

  private def cols_ = height / cellSize

  override def settings(): Unit = {
    size(720, 720, JAVA2D)
    noSmooth()
  }

  private val TITLE: String = "Game of Life"

  private def updateTitle(suffix: String = null): Unit = {
    if (suffix == null) {
      surface.setTitle(s"${TITLE}")
    } else {
      surface.setTitle(s"${TITLE}: ${suffix}")
    }
  }

  override def setup(): Unit = {
    updateTitle()
    frameRate(30)
    stroke(COLOR_GRID)
    // Fill in case cells don't cover all the windows
    background(COLOR_DEAD)

    cells = Array.fill(rows_)(Array.fill(cols_)(Cell(Dead, Dead)))
    initializeCells()
  }

  private def coordinatesRows(): Iterator[Int] = (0 until rows_).iterator

  private def coordinatesCols(): Iterator[Int] = (0 until cols_).iterator

  private def coordinates(): Iterator[(Int, Int)] = {
    for {
      x <- coordinatesRows()
      y <- coordinatesCols()
    } yield {
      (x, y)
    }
  }

  override def draw(): Unit = {
    updateTitle(s"${frameRate.formatted("%.1f")} fps")

    drawCell()

    if (!paused) {
      iteration()
    }

    if (paused && mousePressed) {
      toggleCellStateByMouseClick()
    } else if (paused && !mousePressed) {
      saveCells()
    }
  }

  private def toggleCellStateByMouseClick(): Unit = {
    val xCellOver = {
      val cellOver = map(mouseX.toFloat, 0f, width.toFloat, 0f, rows_.toFloat).toInt
      constrain(cellOver, 0, rows_ - 1)
    }
    val yCellOver = {
      val cellOver = map(mouseY.toFloat, 0f, height.toFloat, 0f, cols_.toFloat).toInt
      constrain(cellOver, 0, cols_ - 1)
    }

    cells(xCellOver)(yCellOver).toggle()
    fill(cells(xCellOver)(yCellOver).color())
  }

  private def drawCell() = {
    for {
      (x, y) <- coordinates()
    } {
      fill(cells(x)(y).color())
      rect((x * cellSize).toFloat, (y * cellSize).toFloat, cellSize, cellSize)
    }
  }

  private def saveCells(): Unit = {
    for {
      (x, y) <- coordinates()
    } {
      cells(x)(y).saveState()
    }
  }

  def iteration(): Unit = {
    saveCells()

    for {
      (x, y) <- coordinates()
    } {
      val neighbours = countNeighbours(x, y)
      cells(x)(y).nextState(neighbours)
    }
  }

  final val bounded: (Int, Int) => Seq[Int] = (current: Int, rangeMax: Int) => {
    val limit = rangeMax - 1
    current match {
      case 0       => Array(current, current + 1)
      case `limit` => Array(current - 1, current)
      case _       => Array(current - 1, current, current + 1)
    }
  }

  final val troidal: (Int, Int) => Seq[Int] = (current: Int, rangeMax: Int) => {
    val limit = rangeMax - 1
    current match {
      case 0       => Array(rangeMax - 1, current, current + 1)
      case `limit` => Array(current - 1, current, 0)
      case _       => Array(current - 1, current, current + 1)
    }
  }

  final val boundaryProcessor = troidal

  private def countNeighbours(x: Int, y: Int): Int = {
    val rows = rows_
    val cols = cols_

    var neighbours = 0
    for {
      xx <- boundaryProcessor(x, rows)
      yy <- boundaryProcessor(y, cols) if !(xx == x && yy == y) && cells(xx)(yy).wasActiveLast()
    } {
      neighbours += 1
    }
    neighbours
  }

  private def initializeCells(): Unit = {
    for {
      (x, y) <- coordinates()
    } {
      cells(x)(y).mutate()
    }
  }

  override def keyPressed(): Unit = {
    key match {
      case 'r' | 'R' => initializeCells()
      case ' '       => togglePause()
      case 'c' | 'C' => clearAllCells()
      case _         => // do nothing
    }
  }

  private def togglePause(): Unit = {
    paused = !paused
  }

  private def clearAllCells(): Unit = {
    for {
      (x, y) <- coordinates()
    } {
      cells(x)(y).die()
    }
  }
}
