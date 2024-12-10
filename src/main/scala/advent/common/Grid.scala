package advent.common

object Grid {
  def apply[V](input: Seq[String])(mapping: Char => V): Map[Point, V] = input.indices
    .flatMap(y => input(y).indices.map(x => Point(x, y) -> mapping(input(y)(x))))
    .toMap
}
