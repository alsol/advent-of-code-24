package advent.days

import advent.common.{Grid, Point}
import advent.{Solution, Task}

object Day8 extends Task {

  override def solve(input: List[String]): (Solution, Solution) = {
    val grid = Grid(input)(identity)

    val allPoints = grid.keySet
    val antennas = grid.filter((_, c) => c != '.')
      .groupMap(_._2)(_._1)

    val width = input.size

    val star1 = (l: Point, r: Point) => Seq(l + (l - r))
    val star2 = (l: Point, r: Point) => (0 to width).map(n => l + (l - r) * n)

    val answer = (f: (Point, Point) => Seq[Point]) => traverse(antennas)(f).toSet.count(allPoints.contains)

    (
      () => answer(star1),
      () => answer(star2)
    )
  }

  private def traverse(antennas: Map[Char, Iterable[Point]])(factory: (Point, Point) => Seq[Point]): Iterable[Point] =
    for {
      (_, points) <- antennas
      self <- points
      if points.size > 1
      other <- points
      if other != self
      node <- factory(other, self)
    } yield node

}
