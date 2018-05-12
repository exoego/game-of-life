package net.exoego

import processing.core.PApplet
import processing.core.PApplet._
import processing.core.PConstants._

import scala.util.Random

class Applet extends PApplet {
  private final val COLOR_ALIVE = color(24, 160, 167)
  private final val COLOR_DEAD  = color(0)
  private final val COLOR_GRID  = color(48)

  private final val cellSize = 10

  private var cells: Array[Array[Cell]]       = Array.empty
  private var bufferCells: Array[Array[Cell]] = Array.empty

  private var paused: Boolean = false

  private def rows_ = width / cellSize

  private def cols_ = height / cellSize

  override def settings(): Unit = {
    size(900, 900, JAVA2D)
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

    cells = Array.fill(rows_)(Array.fill(cols_)(Dead))
    bufferCells = Array.fill(rows_)(Array.fill(cols_)(Dead))
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

  private def draw(cell: Cell): Unit = {
    fill(cell match {
      case Dead => COLOR_DEAD
      case Live => COLOR_ALIVE
    })
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

    val cell = bufferCells(xCellOver)(yCellOver).toggle
    cells(xCellOver)(yCellOver) = cell
    draw(cell)
  }

  private def drawCell() = {
    for {
      (x, y) <- coordinates()
    } {
      draw(bufferCells(x)(y))
      rect((x * cellSize).toFloat, (y * cellSize).toFloat, cellSize, cellSize)
    }
  }

  private def saveCells(): Unit = {
    for {
      (x, y) <- coordinates()
    } {
      bufferCells(x)(y) = cells(x)(y)
    }
  }

  def iteration(): Unit = {
    saveCells()

    for {
      (x, y) <- coordinates()
    } {
      val neighbours = countNeighbours(x, y)
      cells(x)(y) = bufferCells(x)(y).nextState(neighbours)
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
      yy <- boundaryProcessor(y, cols) if !(xx == x && yy == y) && bufferCells(xx)(yy).isLive
    } {
      neighbours += 1
    }
    neighbours
  }

  private final val rand: Random              = new Random(java.security.SecureRandom.getInstanceStrong)
  private final val probabilityOfAliveAtStart = 15

  private def initializeCells(): Unit = {
    for {
      (x, y) <- coordinates()
    } {
      if (rand.nextInt(100) <= probabilityOfAliveAtStart) {
        cells(x)(y) = Live
      } else {
        cells(x)(y) = Dead
      }
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
      cells(x)(y) = Dead
    }
  }
}
