package advent.days

import advent.{Solution, Task}

object Day1 extends Task {

  override def solve(input: List[String]): (Solution, Solution) = {
    val (left, right) = input
      .map(i => i.split("\\s+"))
      .map {
        case Array(l, r) => (l.toLong, r.toLong)
      }
      .unzip

      (
        () => left.sorted.zip(right.sorted).map(_ - _).map(Math.abs).sum,
        () => {
          val rightOccurrences = right.groupBy(identity).view.mapValues(_.size).toMap
          left.map(i => (i, rightOccurrences.getOrElse(i, 0)))
            .map(_ * _)
            .sum
        }
      )
  }
}
