package net.exoego

import processing.core.PApplet
import processing.core.PApplet._
import processing.core.PConstants._

import scala.util.Random

class Applet extends PApplet {
  val title = "Game of Life"

  private final val DEAD: Int  = 0
  private final val ALIVE: Int = 1

  private final val COLOR_ALIVE = color(248, 221, 140)
  private final val COLOR_DEAD  = color(0)

  private final val rand: Random = new Random(java.security.SecureRandom.getInstanceStrong)

  private final val cellSize                  = 5
  private final val probabilityOfAliveAtStart = 15

  private var cells: Array[Array[Int]] = Array.empty
  // Buffer to record the state of the cells and use this while changing the others in the interactions
  private var cellsBuffer: Array[Array[Int]] = Array.empty

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

    val rows = rows_
    val cols = cols_

    // Instantiate arrays
    cells = Array.ofDim(rows, cols)
    cellsBuffer = Array.ofDim(rows, cols)

    // This stroke will draw the background grid
    stroke(48)

    initializeCells()

    // Fill in black in case cells don't cover all the windows
    background(0)
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

    if (cellsBuffer(xCellOver)(yCellOver) == ALIVE) {
      cells(xCellOver)(yCellOver) = DEAD
      fill(COLOR_DEAD)
    } else {
      cells(xCellOver)(yCellOver) = ALIVE
      fill(COLOR_ALIVE)
    }
  }

  private def drawCell() = {
    for {
      (x, y) <- coordinates()
    } {
      if (cells(x)(y) == ALIVE) {
        fill(COLOR_ALIVE)
      } else {
        fill(COLOR_DEAD)
      }
      rect((x * cellSize).toFloat, (y * cellSize).toFloat, cellSize, cellSize)
    }
  }

  private def saveCells(): Unit = {
    for {
      (x, y) <- coordinates()
    } {
      cellsBuffer(x)(y) = cells(x)(y)
    }
  }

  def iteration(): Unit = {
    saveCells()

    for {
      (x, y) <- coordinates()
    } {
      val neighbours   = countNeighbours(x, y)
      val currentState = cellsBuffer(x)(y)
      cells(x)(y) = nextState(currentState, neighbours)
    }
  }

  private def countNeighbours(x: Int, y: Int): Int = {
    val rows = rows_
    val cols = cols_

    var neighbours = 0
    for {
      xx <- (x - 1) to (x + 1) if xx >= 0 && xx < rows
      yy <- (y - 1) to (y + 1) if yy >= 0 && yy < cols && !(xx == x && yy == y) && (cellsBuffer(xx)(yy) == ALIVE)
    } {
      neighbours += 1
    }
    neighbours
  }

  private def nextState(currentState: Int, neighbours: Int): Int = {
    currentState match {
      case ALIVE if neighbours < 2 || neighbours > 3 => DEAD
      case DEAD if neighbours == 3                   => ALIVE
      case _                                         => currentState
    }
  }

  private def initializeCells(): Unit = {
    for {
      (x, y) <- coordinates()
    } {
      cells(x)(y) = generateCell()
    }
  }

  private def generateCell(): Int =
    if (rand.nextInt(100) > probabilityOfAliveAtStart) {
      DEAD
    } else {
      ALIVE
    }

  override def keyPressed(): Unit = {
    key match {
      case 'r' | 'R' => initializeCells()
      case ' '       => togglePause()
      case 'c' | 'C' => clearAllCells()
    }
  }

  private def togglePause(): Unit = {
    paused = !paused
  }

  private def clearAllCells(): Unit = {
    for {
      (x, y) <- coordinates()
    } {
      cells(x)(y) = DEAD
    }
  }
}
