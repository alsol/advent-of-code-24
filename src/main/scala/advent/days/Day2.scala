package advent.days

import advent.{Solution, Task}

import scala.annotation.tailrec

object Day2 extends Task {

  override def solve(input: List[String]): (Solution, Solution) = {
    val reports = input
      .map(_.split("\\s+").map(_.toInt).toList)

    (
      () => reports.count(safe),
      () => reports.count(tolerateSafe)
    )
  }

  private def direction(n: List[Int]): Direction = if (n.head < n(1)) Direction.ASC else Direction.DESC

  private def tolerateSafe(numbers: List[Int]): Boolean = {
    val initial = safe(numbers)
    val tolerate = () => numbers.indices.foldLeft(false)((acc, i) => {
      if acc then acc else {
        val patched = numbers.patch(i, Nil, 1)
        safeTail(direction(patched), patched)
      }
    })

    initial || tolerate()
  }

  private def safe(numbers: List[Int]): Boolean = safeTail(direction(numbers), numbers)

  @tailrec
  private def safeTail(direction: Direction, numbers: List[Int]): Boolean = numbers match {
    case Nil => true
    case x :: Nil => true
    case x :: y :: xs =>
      val diff = Math.abs(x - y)
      if (direction == Direction.ASC && x > y) false
      else if (direction == Direction.DESC && x < y) false
      else if (diff < 1 || diff > 3) false
      else safeTail(direction, y :: xs)
  }

  private enum Direction {
    case ASC
    case DESC
  }
}