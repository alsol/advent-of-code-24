import advent.days.TaskRegistry

import java.time.LocalDate
import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val day = args.headOption
      .getOrElse(currentDay)

    val problem = TaskRegistry.tasks.getOrElse(day, throw IllegalArgumentException(s"Task $day not found"))

    val (star1, star2) = problem.solve(input(_.getLines().toList))

    println(s"$day:")

    println(s"⭐ 1: ${withTiming(star1)()}")
    println(s"⭐ 2: ${withTiming(star2)()}")
  }

  private def withTiming[A](f: () => A): () => String = () => {
    val start = System.currentTimeMillis()
    val result = f()
    val end = System.currentTimeMillis()
    s"$result (took ${end - start}ms)"
  }

  private def currentDay: String = s"Day${LocalDate.now.getDayOfMonth}"

  private def input[A](f: Source => A): A = closeable(Source.fromFile("./input.txt"))(f)

  private def closeable[A](r: Source)(f: Source => A): A =
    try {
      f(r)
    } finally {
      r.close()
    }
}