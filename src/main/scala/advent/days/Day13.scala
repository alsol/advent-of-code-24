package advent.days

import advent.{Solution, Task}


object Day13 extends Task {

  override def solve(input: List[String]): (Solution, Solution) = {

    val button = "Button [AB]: X\\+(\\d+),\\s?Y\\+(\\d+)".r
    val prize = "Prize: X=(\\d+),\\s?Y=(\\d+)".r

    val points = input
      .map {
        case button(x, y) => (x.toLong, y.toLong)
        case prize(x, y) => (x.toLong, y.toLong)
        case _ => (0L, 0L)
      }

    val claws = input.zipWithIndex
      .flatMap((line, i) => {
        if (line.contains("Prize:")) {
          val (xa, ya) = points(i - 2)
          val (xb, yb) = points(i - 1)
          val (xf, yf) = points(i)
          Equations(xa, ya, xb, yb, xf, yf) :: Nil
        } else {
          Nil
        }
      })

    (
      () => claws.flatMap(_.solve).sum,
      () => claws.map(c => c.copy(xf = c.xf + 10000000000000L, yf = c.yf + 10000000000000L))
        .flatMap(_.solve).sum
    )
  }

  private case class Equations(xa: Long, ya: Long, xb: Long, yb: Long, xf: Long, yf: Long) {

    /**
     * Linear system of 2 equations
     * | xa * a + xb * b = xf
     * | ya * a + yb * b = yf
     */
    def solve: Option[Long] = {
      val det = xa * yb - xb * ya
      if (det == 0) {
        return None
      }

      val detX = xf * yb - xb * yf
      val detY = xa * yf - ya * xf

      if (detX % det == 0 && detY % det == 0) {
        Some((detX / det) * 3 + (detY / det))
      } else {
        None
      }
    }
  }

}
