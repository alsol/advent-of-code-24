package advent.days

import advent.{Solution, Task}

import scala.annotation.tailrec

object Day5 extends Task {

  override def solve(input: List[String]): (Solution, Solution) = {
    val rules = input
      .takeWhile(!_.isBlank)
      .map(_.split('|'))
      .map {
        case Array(l, r) => Rule(l.toInt, r.toInt)
      }

    val updates = input
      .drop(rules.length + 1)
      .map(_.split(',').map(_.toInt).toList)

    (
      () => updates.filter(isValid(_, rules)).map(update => update(update.length / 2)).sum,
      () => updates.filter(!isValid(_, rules)).map(sort(_, rules)).map(update => update(update.length / 2)).sum
    )
  }

  private case class Rule(left: Int, right: Int)

  private def isValid(update: List[Int], rules: List[Rule]): Boolean = {
    for (rule <- rules) {
      val left = update.indexOf(rule.left)
      val right = update.indexOf(rule.right)

      if (left != -1 && right != -1 && left > right) {
        return false
      }
    }

    true
  }

  @tailrec
  private def sort(update: List[Int], rules: List[Rule]): List[Int] = {
    if (isValid(update, rules)) {
      return update
    }

    val swap = (
      for {
        rule <- rules
        left = update.indexOf(rule.left)
        right = update.indexOf(rule.right)
        if left != -1 && right != -1 && left > right
      } yield (left, right)
      ).headOption

    val sorted = update.indices
      .map(i => {
        swap match {
          case Some((l, r)) if l == i => update(r)
          case Some((l, r)) if r == i => update(l)
          case _ => update(i)
        }
      })
      .toList

    sort(sorted, rules)
  }
}
