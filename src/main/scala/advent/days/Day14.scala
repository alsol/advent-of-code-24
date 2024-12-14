package advent.days

import advent.common.Point
import advent.{Solution, Task}


object Day14 extends Task {

  private val width = 101
  private val height = 103

  override def solve(input: List[String]): (Solution, Solution) = {

    val re = "p=(.+),(.+)\\sv=(.+),(.+)".r

    val robots = input.map {
      case re(x, y, vx, vy) => Robot(Point(x.toInt, y.toInt), Point(vx.toInt, vy.toInt))
    }

    (
      () => star1(robots),
      () => star2(robots)
    )
  }

  private def star1(robots: List[Robot]): Int = {
    val (midX, midY) = (width / 2, height / 2)
    val steps = 100

    def quadrant: Point => Int = p => {
      val (x, y) = (p.x, p.y)
      if (x == midX || y == midY) 0
      else if (x < midX && y < midY) 1
      else if (x < midX && y >= midY) 2
      else if (x >= midX && y < midY) 3
      else 4
    }

    robots.map(_.simulate(steps))
      .groupMapReduce(quadrant)(_ => 1)(_ + _)
      .filter((q, _) => q > 0)
      .map((_, c) => c)
      .product
  }

  private def star2(robots: List[Robot]): Int = {
    var overlap = true
    var iterations = 0
    var state = robots

    while (overlap) {
      state = state.map(_.mov)
      iterations += 1
      overlap = state.groupMapReduce(_.position)(_ => 1)(_ + _).exists((_, c) => c > 1)
    }

    iterations
  }

  private case class Robot(position: Point, velocity: Point) {
    def simulate(steps: Int): Point = {
      val predict = position + (velocity * steps)
      Point(cycle(predict.x, width), cycle(predict.y, height))
    }

    def mov: Robot = copy(position = simulate(1))
  }

  private def cycle(x: Int, n: Int): Int = ((x % n) + n) % n
}
