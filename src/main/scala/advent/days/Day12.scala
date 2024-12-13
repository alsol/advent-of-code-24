package advent.days

import advent.common.Point
import advent.{Solution, Task}

import scala.collection.mutable

object Day12 extends Task {

  override def solve(input: List[String]): (Solution, Solution) = {
    val grid = advent.common.Grid(input)(identity)

    (
      () => price(grid)(calculateAreaAndPerimeter),
      () => price(grid)(calculateAreaAndSides)
    )
  }

  private type Collector = (Map[Point, Char], Point, mutable.Set[Point]) => (Int, Int)

  private def price(regions: Map[Point, Char])(collector: Collector): Long = {
    val visited = mutable.Set[Point]()
    var totalPrice = 0

    for (point <- regions.keySet if !visited.contains(point)) {
      val (area, surroundings) = collector(regions, point, visited)
      totalPrice += area * surroundings
    }

    totalPrice
  }

  private def calculateAreaAndPerimeter(regions: Map[Point, Char], start: Point, visited: mutable.Set[Point]): (Int, Int) = {
    val directions = List(Point(0, 1), Point(1, 0), Point(0, -1), Point(-1, 0))
    val queue = mutable.Queue[Point](start)
    val regionType = regions(start)
    var perimeter = 0
    var area = 0

    while (queue.nonEmpty) {
      val current = queue.dequeue()
      if (!visited.contains(current)) {
        visited.add(current)
        area += 1

        for (dir <- directions) {
          val neighbor = Point(current.x + dir.x, current.y + dir.y)
          if (regions.getOrElse(neighbor, '@') != regionType) {
            perimeter += 1
          } else if (!visited.contains(neighbor)) {
            queue.enqueue(neighbor)
          }
        }
      }
    }

    (area, perimeter)
  }

  private def calculateAreaAndSides(regions: Map[Point, Char], start: Point, visited: mutable.Set[Point]): (Int, Int) = {
    val directions = List(Point(0, 1), Point(1, 0), Point(0, -1), Point(-1, 0))
    val regionType = regions(start)
    val region = mutable.Set[Point]()

    val queue = mutable.Queue[Point](start)
    while (queue.nonEmpty) {
      val current = queue.dequeue()
      if (!region.contains(current)) {
        visited.add(current)
        region.add(current)

        for (dir <- directions) {
          val neighbor = Point(current.x + dir.x, current.y + dir.y)
          if (regions.getOrElse(neighbor, '@') == regionType && !region.contains(neighbor)) {
            queue.enqueue(neighbor)
            visited.add(neighbor)
            region.add(current)
          }
        }
      }
    }

    var sides = 0

    for (dir <- directions) {
      val processed = mutable.Set[Point]()
      for (p <- region if !processed.contains(p)) {
        processed.add(p)
        if (regions.getOrElse(p + dir, '@') != regionType) {
          sides += 1

          var pointer = p
          for (rotation <- List(Point(dir.y, dir.x), Point(-dir.y, -dir.x))) {
            while ( {
              pointer += rotation
              regions.getOrElse(pointer, '@') == regionType && regions.getOrElse(pointer + dir, '@') != regionType
            }) {
              processed.add(pointer)
            }
          }
        }
      }
    }

    (region.size, sides)
  }
}
