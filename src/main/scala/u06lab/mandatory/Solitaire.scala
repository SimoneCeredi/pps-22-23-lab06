package u06lab.mandatory

object Solitaire extends App:
  type Mark = (Int, Int)
  type Solution = List[Mark]
  type IterableFactory = Solution => Seq[Solution]
  val w = 5
  val h = 5

  val moves = List((2, 2), (2, -2), (-2, -2), (-2, 2), (0, 3), (3, 0), (-3, 0), (0, -3))

  given IterableFactory = Seq(_)

  placeMarks().zipWithIndex foreach (s => println(s"\nSol #${s._2}\n${render(s._1, w, h)}"))

  def placeMarks(solLength: Int = w * h)(using factory: IterableFactory): Seq[Solution] =
    solLength match
      case 1 => factory(List((w / 2, h / 2)))
      case _ =>
        for
          solution <- placeMarks(solLength - 1)
          move <- moves
          lastMark = solution.last
          mark = (lastMark._1 + move._1, lastMark._2 + move._2)
          if isLegitimate(mark, solution)
        yield
          solution :+ mark

  def isLegitimate(mark: Mark, others: Solution): Boolean =
    isInsideBoard(mark) && !(others contains mark)

  def isInsideBoard(mark: Mark): Boolean = mark._1 >= 0 && mark._1 < w && mark._2 >= 0 && mark._2 < h

  def render(solution: Solution, width: Int, height: Int): String =
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
                    number = solution.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")


