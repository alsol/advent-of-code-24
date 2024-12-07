package advent.days

import advent.{Solution, Task}

object Day7 extends Task {

  override def solve(input: List[String]): (Solution, Solution) = {
    val equations = input
      .map(_.split(": "))
      .map {
        case Array(test, values) => Equation(test.toLong, values.split(" ").map(_.toLong).toList)
      }

    (
      () => equations.filter(equal(_)(List(plus, multiply))).map(_.test).sum,
      () => equations.filter(equal(_)(List(concat, plus, multiply))).map(_.test).sum,
    )
  }

  private case class Equation(test: Long, values: List[Long])

  private def equal(eq: Equation)(implicit operators: List[Operator]): Boolean = equal(eq.test, eq.values)

  private def equal(test: Long, values: List[Long])(implicit operators: List[Operator]): Boolean =
    values match {
      case l :: Nil => test == l
      case l :: r :: tail => operators.exists(op => equal(test, op(l, r) :: tail))
    }

  private type Operator = (Long, Long) => Long

  private def plus: Operator = (l, r) => l + r

  private def multiply: Operator = (l, r) => l * r

  private def concat: Operator = (l, r) => (l.toString + r.toString).toLong
}
