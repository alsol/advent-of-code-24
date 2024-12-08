package advent.common

import scala.annotation.targetName

case class Point(x: Int, y: Int) {
  @targetName("+")
  def +(other: Point): Point = Point(x + other.x, y + other.y)

  @targetName("-")
  def -(other: Point): Point = Point(x - other.x, y - other.y)

  @targetName("*")
  def *(factor: Int): Point = Point(x * factor, y * factor)
}
