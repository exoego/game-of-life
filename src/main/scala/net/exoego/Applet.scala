package net.exoego

import processing.core.PApplet
import processing.core.PApplet._
import processing.core.PConstants._

import scala.util.Random

class Applet extends PApplet {
  val title = "Game of Life"

  private final val DEAD: Int  = 0
  private final val ALIVE: Int = 1

  private final val rand: Random = new Random(java.security.SecureRandom.getInstanceStrong)

  private final val cellSize                  = 5
  private final val probabilityOfAliveAtStart = 15

  private final val alive = color(248, 221, 140)
  private final val dead  = color(0)

  private var cells: Array[Array[Int]] = Array.empty
  // Buffer to record the state of the cells and use this while changing the others in the interations
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

  private def iterate(): Iterator[(Int, Int)] = {
    for {
      x <- (0 until rows_).iterator
      y <- (0 until cols_).iterator
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
    // Map and avoid out of bound errors
    val xCellOver = {
      val cellOver = map(mouseX.toFloat, 0f, width.toFloat, 0f, rows_.toFloat).toInt
      constrain(cellOver, 0, rows_ - 1)
    }
    val yCellOver = {
      val cellOver = map(mouseY.toFloat, 0f, height.toFloat, 0f, cols_.toFloat).toInt
      constrain(cellOver, 0, cols_ - 1)
    }

    // Check against cells in buffer
    if (cellsBuffer(xCellOver)(yCellOver) == 1) {
      cells(xCellOver)(yCellOver) = DEAD
      fill(dead)
    } else { // Cell is dead
      cells(xCellOver)(yCellOver) = ALIVE
      fill(alive)
    }
  }

  private def drawCell() = {
    for {
      (x, y) <- iterate()
    } {
      if (cells(x)(y) == 1) {
        fill(alive); // If alive
      } else {
        fill(dead); // If dead
      }
      rect((x * cellSize).toFloat, (y * cellSize).toFloat, cellSize, cellSize)
    }
  }

  private def saveCells(): Unit = {
    // Save cells to buffer (so we opeate with one array keeping the other intact)
    for {
      (x, y) <- iterate()
    } {
      cellsBuffer(x)(y) = cells(x)(y)
    }
  }

  def iteration(): Unit = {
    val rows = rows_
    val cols = cols_

    saveCells()

    for {
      (x, y) <- iterate()
    } {
      var neighbours = 0

      for {
        xx <- x - 1 to x + 1 if xx >= 0 && xx < rows
        yy <- y - 1 to y + 1 if yy >= 0 && yy < cols && !(xx == x && yy == y)
      } {
        // Check alive neighbours and count them
        if (cellsBuffer(xx)(yy) == 1) {
          neighbours += 1
        }
      }

      if (cellsBuffer(x)(y) == ALIVE) {
        if (neighbours < 2 || neighbours > 3) {
          cells(x)(y) = DEAD
        }
      } else {
        if (neighbours == 3) {
          cells(x)(y) = ALIVE
        }
      }
    }
  }

  private def initializeCells(): Unit = {
    for {
      (x, y) <- iterate()
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
    if (key == 'r' || key == 'R') {
      initializeCells()
    }

    if (key == ' ') { // On/off of pause
      paused = !paused

    }
    if (key == 'c' || key == 'C') { // Clear all
      for {
        (x, y) <- iterate()
      } {
        cells(x)(y) = DEAD
      }
    }
  }

}
