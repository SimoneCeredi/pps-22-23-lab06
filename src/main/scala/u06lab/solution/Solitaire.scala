package u06lab.solution

import u06lab.solution.Solitaire.{h, w}

object Solitaire extends App:
  type Mark = (Int, Int)
  type Solution = List[Mark]
  type IterableFactory = Solution => Seq[Solution]
  val w = 5
  val h = 5

  val moves = List((2, 2), (2, -2), (-2, -2), (-2, 2), (0, 3), (3, 0), (-3, 0), (0, -3))

  given IterableFactory = Seq(_)

  val sol = placeMarks()
  sol.zipWithIndex foreach printSolution

  def placeMarks(w: Int = w, h: Int = h, solLength: Int = w * h)(using factory: IterableFactory): Seq[Solution] =
    solLength match
      case 1 => factory(List((w / 2, h / 2)))
      case _ =>
        for
          solution <- placeMarks(solLength = solLength - 1)
          move <- moves
          lastMark = solution.last
          mark = (lastMark._1 + move._1, lastMark._2 + move._2)
          if isLegitimate(mark, solution)
        yield
          solution :+ mark

  def isLegitimate(mark: Mark, others: Solution): Boolean =
    mark._1 >= 0 && mark._1 < w && mark._2 >= 0 && mark._2 < h
      && !(others contains mark)

  def printSolution(si: (Solution, Int)): Unit =
    println(s"sol ${si._2}")
    println(si._1)

