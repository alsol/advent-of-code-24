package advent.days

import advent.common.{Grid, Point}
import advent.{Solution, Task}

import scala.collection.mutable

object Day10 extends Task {

  private val surroundings: List[Point] = Point(1, 0) :: Point(-1, 0) :: Point(0, 1) :: Point(0, -1) :: Nil

  override def solve(input: List[String]): (Solution, Solution) = {
    val grid = Grid(input)(_.asDigit)

    val star1: Point => Int = point => {
      val state = mutable.Set[Point]()
      calculateScore(point, grid) { p =>
        val seen = state.contains(p)
        state.add(p)
        seen
      }
    }

    val star2: Point => Int = p => calculateScore(p, grid)(_ => false)

    (
      () => grid.filter(_._2 == 0).map { case (point, _) => star1(point) }.sum,
      () => grid.filter(_._2 == 0).map { case (point, _) => star2(point) }.sum
    )
  }

  private def calculateScore(start: Point, grid: Map[Point, Int])(implicit visited: Point => Boolean): Int = {
    val current = grid(start)

    if (current == 9) {
      return if (!visited(start)) 1 else 0
    }

    val nextPoints = surroundings
      .map(start + _)

    val scores = for {
      point <- nextPoints
      if grid.contains(point)
      if grid(point) == current + 1
    } yield calculateScore(point, grid)

    scores.sum
  }
}
