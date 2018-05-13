package net.exoego

import processing.core.PApplet
import processing.core.PApplet._
import processing.core.PConstants._

import java.util.concurrent.{ConcurrentHashMap => ConcurrentHashMap}

import scala.collection.parallel.mutable.{ParSet => MutableParSet}
import scala.util.Random

class Applet extends PApplet {
  private final val COLOR_ALIVE = color(24, 160, 167)
  private final val COLOR_DEAD  = color(0)
  private final val COLOR_GRID  = color(48)

  private final val cellSize                  = 5
  private final val probabilityOfAliveAtStart = 15
  private final val useFullScreen             = true

  private final val cells       = MutableParSet.empty[(Int, Int)]
  private final val bufferCells = MutableParSet.empty[(Int, Int)]
  private final val countCells  = new ConcurrentHashMap[(Int, Int), Int]()

  private final val lifecycleRule: LifeCycleRule         = Rule23_3
  private final val boundaryProcessor: BoundaryProcessor = Loop

  private final val rand: Random = new Random(java.security.SecureRandom.getInstanceStrong)

  private var paused: Boolean = false

  private def rows_ = width / cellSize

  private def cols_ = height / cellSize

  override def settings(): Unit = {
    if (useFullScreen) {
      fullScreen(JAVA2D)
    } else {
      size(1600, 900, JAVA2D)
    }
    noSmooth()
  }

  private val TITLE: String = "Game of Life"

  private def updateTitle(fps: String = null): Unit = {
    if (fps == null) {
      surface.setTitle(s"${TITLE}")
    } else {
      surface.setTitle(s"${TITLE}: ${if (paused) "paused" else fps}")
    }
  }

  override def setup(): Unit = {
    updateTitle()
    frameRate(30)
    stroke(COLOR_GRID)
    background(COLOR_DEAD)
    initializeCells()
  }

  override def draw(): Unit = {
    updateTitle(s"${frameRate.formatted("%.1f")} fps")

    drawCell()

    if (!paused) {
      saveCells()
      iteration()
    } else if (mousePressed) {
      toggleCellStateByMouseClick()
    } else {
      lastClickedIsAlive = None
      saveCells()
    }
  }

  private var lastClickedIsAlive: Option[Boolean] = None

  private def toggleCellStateByMouseClick(): Unit = {
    val xCellOver = {
      val cellOver = map(mouseX.toFloat, 0f, width.toFloat, 0f, rows_.toFloat).toInt
      constrain(cellOver, 0, rows_ - 1)
    }
    val yCellOver = {
      val cellOver = map(mouseY.toFloat, 0f, height.toFloat, 0f, cols_.toFloat).toInt
      constrain(cellOver, 0, cols_ - 1)
    }

    val isAlive = lastClickedIsAlive match {
      case Some(lastIsAlive) => lastIsAlive
      case None =>
        val isAlive = bufferCells.contains((xCellOver, yCellOver))
        lastClickedIsAlive = Some(isAlive)
        isAlive
    }
    toggle(xCellOver, yCellOver, isAlive)
  }

  private def toggle(x: Int, y: Int, isAlive: Boolean): Unit = {
    if (isAlive) {
      cells -= ((x, y))
      bufferCells -= ((x, y))
      draw(x, y, false)
    } else {
      cells += ((x, y))
      bufferCells += ((x, y))
      draw(x, y, true)
    }
  }

  private def updateNeighbours(x: Int, y: Int, n: Int): Unit = {
    val rows = rows_
    val cols = cols_

    for {
      xx <- boundaryProcessor(x, rows)
      yy <- boundaryProcessor(y, cols) if !(xx == x && yy == y)
    } {
      countCells.compute((xx, yy), (_, old) => {
        old + n
      })
    }
  }

  private def drawCell(): Unit = {
    fillColor(false)
    rect(0f, 0f, width.toFloat, height.toFloat)

    for {
      (x, y) <- bufferCells.iterator
    } {
      draw(x, y, true)
    }
  }

  private def draw(x: Int, y: Int, isAlive: Boolean): Unit = {
    fillColor(isAlive)
    rect((x * cellSize).toFloat, (y * cellSize).toFloat, cellSize, cellSize)
  }

  private def fillColor(isAlive: Boolean): Unit = {
    fill(isAlive match {
      case false => COLOR_DEAD
      case true  => COLOR_ALIVE
    })
  }

  private def saveCells(): Unit = {
    bufferCells.clear()
    for {
      (x, y) <- cells.par.iterator
    } {
      bufferCells += ((x, y))
    }
    countCells.clear()
  }

  def iteration(): Unit = {
    bufferCells.foreach {
      case (x, y) =>
        updateNeighbours(x, y, 1)
    }

    cells.clear()
    countCells.entrySet().forEach { t =>
      val coordinate = t.getKey
      val neighbours = t.getValue
      if (lifecycleRule(bufferCells.contains(coordinate), neighbours)) {
        cells += coordinate
      } else {
        cells -= coordinate
      }
    }
  }

  private def initializeCells(): Unit = {
    val size  = (width * height) / (cellSize * cellSize) * probabilityOfAliveAtStart / 100
    val chunk = 1

    val sizePerChunk = size / chunk

    clearAllCells()
    (0 until chunk).par.foreach { _ =>
      val randomCoordinates = Stream.continually(rand).map(r => (r.nextInt(rows_), r.nextInt(cols_))).take(sizePerChunk)
      for {
        coordinate <- randomCoordinates
      } {
        cells += coordinate
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
