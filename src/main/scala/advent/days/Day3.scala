package advent.days

import advent.{Solution, Task}

object Day3 extends Task {

  override def solve(input: List[String]): (Solution, Solution) = {
    val re = "(do)\\(\\)|(don't)\\(\\)|(mul)\\((\\d+),(\\d+)\\)".r

    val ops = input
      .flatMap(re.findAllMatchIn(_).toList)
      .map {
        case re("do", _, _, _, _) => Operation.Do
        case re(_, "don't", _, _, _) => Operation.Dont
        case re(_, _, "mul", a, b) => Operation.Mul(a.toInt, b.toInt)
      }

    (
      () => ops.map {
        case Operation.Mul(a, b) => (a, b)
        case _ => (0, 0)
      }.map(_ * _).sum,
      
      () => {
        case class State(allow: Boolean, sum: Long) {
          def sum(sum: Long): State = copy(sum = this.sum + sum)
        }

        ops.foldLeft(State(true, 0)) { (state, op) =>
            op match {
              case Operation.Do => state.copy(allow = true)
              case Operation.Dont => state.copy(allow = false)
              case Operation.Mul(a, b) => if (state.allow) state.sum(a * b) else state
            }
          }
          .sum
      }
    )
  }

  private enum Operation {
    case Do
    case Dont
    case Mul(a: Int, b: Int)
  }
}
