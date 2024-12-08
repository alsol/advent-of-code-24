package advent.days

import advent.common.Point
import advent.{Solution, Task}

object Day8 extends Task {

  override def solve(input: List[String]): (Solution, Solution) = {
    val grid = input.indices
      .flatMap(y => input(y).indices.map(x => Point(x, y) -> input(y)(x)))
      .toMap

    val allPoints = grid.keySet
    val antennas = grid.filter((_, c) => c != '.')
      .groupMap(_._2)(_._1)

    val width = input.size

    val star1 = (l: Point, r: Point) => Seq(l + (l - r))
    val star2 = (l: Point, r: Point) => (-width to width).map(n => l + (l - r) * n)

    val answer = (f: (Point, Point) => Seq[Point]) => traverse(antennas)(f).toSet.intersect(allPoints).size

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
