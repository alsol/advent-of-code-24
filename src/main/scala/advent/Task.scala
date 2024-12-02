package advent

type Solution = () => Long

trait Task {

  def solve(input: List[String]): (Solution, Solution)
  
}
