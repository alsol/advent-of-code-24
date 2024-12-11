package advent.days

import advent.{Solution, Task}

import scala.annotation.tailrec
import scala.collection.mutable

object Day11 extends Task {

  override def solve(input: List[String]): (Solution, Solution) = {
    val stones = input.head
      .split(" ")
      .map(s => s.toLong -> 1L)
      .toMap

    (
      () => blink(stones, 25),
      () => blink(stones, 75)
    )
  }

  @tailrec
  private def blink(stones: Map[Long, Long], max: Long, iteration: Long = 0): Long = {
    if (iteration == max) {
      return stones.values.sum
    }

    val newStones: mutable.Map[Long, Long] = mutable.Map().withDefaultValue(0)

    stones.foreach{
      case (stone, count) if stone == 0 => newStones(1) += count
      case (stone, count) if stone.toString.length % 2 == 0 =>
        val (left, right) = stone.toString.splitAt(stone.toString.length / 2)
        newStones(left.toLong) += count
        newStones(right.toLong) += count
      case (stone, count) => newStones(stone * 2024L) += count
    }

    blink(newStones.toMap, max, iteration + 1)
  }
}
