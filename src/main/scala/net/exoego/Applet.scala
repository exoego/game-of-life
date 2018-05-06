package net.exoego

import processing.core.PApplet
import processing.core.PConstants._

class Applet extends PApplet {
  private final val BG_COLOR: Int = {
    color(0, 0, 0)
  }

  override def settings(): Unit = {
    size(640, 480, JAVA2D)
  }

  val title = "Game of Life"

  override def setup(): Unit = {
    surface.setTitle(title)
    smooth()
    frameRate(60)
  }

  override def draw(): Unit = {
    background(BG_COLOR)

    textAlign(LEFT, TOP)
    text(s"$mouseX, $mouseY", 0, 0)

    textAlign(CENTER, CENTER)
  }
}
