package advent.days

import advent.common.{Grid, Point}
import advent.{Solution, Task}

import scala.annotation.tailrec
import scala.collection.mutable

object Day6 extends Task {

  override def solve(input: List[String]): (Solution, Solution) = {
    val grid: Map[Point, State] = Grid(input)(initial)

    val start: Point = grid.find((_, state) => state match {
      case State.Guard(_) => true
      case _ => false
    }).get._1

    (
      () => walkAround(grid, start).count((_, state) => state == State.Visited),
      () => grid.keys
        .filter(_ != start)
        .filter(grid(_) match {
          case State.Empty => true
          case _ => false
        })
        .map(grid.updated(_, State.Obstacle))
        .count(walkAround(_, start) == null)
    )
  }

  @tailrec
  private def walkAround(grid: Map[Point, State], guardPos: Point)(implicit cycle: mutable.Map[Point, mutable.HashSet[Point]] = new mutable.HashMap()): Map[Point, State] = {
    if (!grid.contains(guardPos)) {
      return grid
    }

    val guard: State.Guard = grid.get(guardPos) match {
      case Some(value: State.Guard) => value
      case _ => State.Guard(Direction.Up)
    }

    val guardNextStep = guardPos + guard.direction.forward

    val nextStep = grid.getOrElse(guardNextStep, guard)

    if (cycle.getOrElse(guardPos, Set()).contains(guardNextStep)) {
      return null
    }

    cycle.getOrElseUpdate(guardPos, new mutable.HashSet[Point]).add(guardNextStep)

    nextStep match {
      case State.Empty | State.Visited => walkAround(grid.updated(guardPos, State.Visited).updated(guardNextStep, guard), guardNextStep)
      case State.Obstacle => walkAround(grid.updated(guardPos, State.Guard(guard.direction.turn)), guardPos)
      case _ => walkAround(grid.updated(guardPos, State.Visited), guardNextStep)
    }
  }

  private def initial(char: Char): State = char match {
    case '#' => State.Obstacle
    case '^' => State.Guard(Direction.Up)
    case _ => State.Empty
  }

  private enum State {
    case Empty
    case Visited
    case Obstacle
    case Guard(direction: Direction)
  }

  private enum Direction {
    case Up
    case Down
    case Left
    case Right;

    def turn: Direction = this match {
      case Up => Right
      case Right => Down
      case Down => Left
      case Left => Up
    }

    def forward: Point = this match {
      case Up => Point(0, -1)
      case Down => Point(0, 1)
      case Left => Point(-1, 0)
      case Right => Point(1, 0)
    }
  }
}