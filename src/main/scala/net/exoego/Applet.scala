package net.exoego

import processing.core.PApplet
import processing.core.PApplet._
import processing.core.PConstants._

import scala.collection.mutable
import scala.util.Random

class Applet extends PApplet {
  private final val COLOR_ALIVE = color(24, 160, 167)
  private final val COLOR_DEAD  = color(0)
  private final val COLOR_GRID  = color(48)

  private final val cellSize                           = 4
  private val cells: mutable.Set[(Int, Int)]           = mutable.Set.empty
  private val bufferCells: mutable.Set[(Int, Int)]     = mutable.Set.empty
  private val countCells: mutable.Map[(Int, Int), Int] = mutable.Map.empty

  private var paused: Boolean = false

  private def rows_ = width / cellSize

  private def cols_ = height / cellSize

  override def settings(): Unit = {
    size(1600, 900, JAVA2D)
    noSmooth()
  }

  private val TITLE: String = "Game of Life"

  private def updateTitle(suffix: String = null): Unit = {
    if (suffix == null) {
      surface.setTitle(s"${TITLE}")
    } else {
      surface.setTitle(s"${TITLE}: ${suffix} ${if (paused) ": paused" else ""}")
    }
  }

  override def setup(): Unit = {
    updateTitle()
    frameRate(30)
    stroke(COLOR_GRID)
    // Fill in case cells don't cover all the windows
    background(COLOR_DEAD)
    initializeCells()
  }

  private def coordinatesRows(): Iterator[Int] = (0 until rows_).iterator

  private def coordinatesCols(): Iterator[Int] = (0 until cols_).iterator

  private def allCoordinates(): Iterator[(Int, Int)] = {
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

  private def draw(isAlive: Boolean): Unit = {
    fill(isAlive match {
      case false => COLOR_DEAD
      case true  => COLOR_ALIVE
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

    toggle(xCellOver, yCellOver)
  }

  private def toggle(x: Int, y: Int): Unit = {
    if (bufferCells.contains((x, y))) {
      bufferCells.remove((x, y))
      cells.remove((x, y))
      draw(false)
    } else {
      bufferCells.add((x, y))
      cells.add((x, y))
      draw(true)
    }
  }

  private def updateNeighbours(x: Int, y: Int, n: Int): Unit = {
    val rows = rows_
    val cols = cols_

    for {
      xx <- boundaryProcessor(x, rows)
      yy <- boundaryProcessor(y, cols) if !(xx == x && yy == y)
    } {
      val u = countCells.getOrElse((xx, yy), 0) + n
      if (u == 0) {
        countCells.remove((xx, yy))
      } else {
        countCells.update((xx, yy), u)
      }
    }
  }

  private def drawCell(): Unit = {
    draw(false)
    rect(0f, 0f, width.toFloat, height.toFloat)

    for {
      (x, y) <- bufferCells.iterator
    } {
      draw(true)
      rect((x * cellSize).toFloat, (y * cellSize).toFloat, cellSize, cellSize)
    }
  }

  private def saveCells(): Unit = {
    bufferCells.clear()
    for {
      (x, y) <- cells.iterator
    } {
      bufferCells.add((x, y))
    }
    countCells.clear()
  }

  def iteration(): Unit = {
    saveCells()

    bufferCells.iterator.foreach {
      case (x, y) =>
        updateNeighbours(x, y, 1)
    }

    cells.clear()
    countCells.foreach {
      case ((x, y), neighbours) =>
        if (rule(bufferCells.contains((x, y)), neighbours)) {
          cells.add((x, y))
        } else {
          cells.remove((x, y))
        }
    }
  }

  final val rule: (Boolean, Int) => Boolean = (isAlive: Boolean, neighbours: Int) => {
    (isAlive, neighbours) match {
      case (true, 2)  => true
      case (true, 3)  => true
      case (false, 3) => true
      case _          => false
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

  private final val rand: Random              = new Random(java.security.SecureRandom.getInstanceStrong)
  private final val probabilityOfAliveAtStart = 15

  private def initializeCells(): Unit = {
    clearAllCells()
    for {
      (x, y) <- allCoordinates()
    } {
      if (rand.nextInt(100) <= probabilityOfAliveAtStart) {
        cells.add((x, y))
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
    cells.clear()
    bufferCells.clear()
    countCells.clear()
  }
}
