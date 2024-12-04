package advent.days

import advent.common.Point
import advent.{Solution, Task}

object Day4 extends Task {

  override def solve(input: List[String]): (Solution, Solution) = {
    val grid = input.indices
      .flatMap(y => input(y).indices.map(x => Point(x, y) -> input(y)(x)))
      .toMap

    (
      () => star1(grid),
      () => star2(grid)
    )
  }

  private def star1(grid: Map[Point, Char]): Long = {
    val startPositions = grid.filter(_._2 == 'X').keys.toList

    (for {
      start <- startPositions
      direction <- List(up(start), down(start), left(start), right(start), upRight(start), upLeft(start), downRight(start), downLeft(start))
      if direction.map(grid.getOrElse(_, '.')).mkString == "MAS"
    } yield 1).sum
  }

  private def star2(grid: Map[Point, Char]): Long = {
    val startPositions = grid.filter(_._2 == 'A').keys.toList

    val right = List(Point(-1, -1), Point(0, 0), Point(1, 1))
    val left = List(Point(-1, 1), Point(0, 0), Point(1, -1))

    (
      for {
        start <- startPositions
      } yield {
        val rightDiagonal = right.map(start + _).map(grid.getOrElse(_, '.')).mkString
        val leftDiagonal = left.map(start + _).map(grid.getOrElse(_, '.')).mkString

        if ((rightDiagonal == "MAS" || rightDiagonal == "SAM") && (leftDiagonal == "MAS" || leftDiagonal == "SAM"))
          1
        else
          0
      }
      ).sum
  }

  private def up(start: Point): List[Point] = List(1, 2, 3)
    .map(Point(0, _))
    .map(start + _)

  private def down(start: Point): List[Point] = List(1, 2, 3)
    .map(Point(0, _))
    .map(start - _)

  private def left(start: Point): List[Point] = List(1, 2, 3)
    .map(Point(_, 0))
    .map(start - _)

  private def right(start: Point): List[Point] = List(1, 2, 3)
    .map(Point(_, 0))
    .map(start + _)

  private def upRight(start: Point): List[Point] = List(1, 2, 3)
    .map(i => Point(i, i))
    .map(start + _)

  private def upLeft(start: Point): List[Point] = List(1, 2, 3)
    .map(i => Point(-i, i))
    .map(start + _)

  private def downRight(start: Point): List[Point] = List(1, 2, 3)
    .map(i => Point(i, -i))
    .map(start + _)

  private def downLeft(start: Point): List[Point] = List(1, 2, 3)
    .map(i => Point(-i, -i))
    .map(start + _)
}
