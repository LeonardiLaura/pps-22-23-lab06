package u06lab.solutions

import jdk.internal.org.jline.utils.WCWidth

object Solitaire extends App:
  def render(solution: Seq[(Int, Int)], width: Int, height: Int): String =
    val reversed = solution.reverse
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
          number = reversed.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")

  def legalMove(pos: (Int, Int), move: (Int, Int)): Boolean =
    (pos._1 == move._1 && (pos._2 - move._2).abs.equals(3)) ||
      (pos._2 == move._2 && (pos._1 - move._1).abs.equals(3)) ||
      ((pos._1 - move._1).abs.equals(2) && (pos._2 - move._2).abs.equals(2))

  def placeMarks(w: Int, h: Int): Iterable[Seq[(Int,Int)]] =
    def _placeMarks(size: Int): Iterable[Seq[(Int,Int)]] = size match
      case 1 => Iterable(Seq((w / 2, h / 2)))
      case _ =>
        for
          marks <- _placeMarks(size - 1)
          x <- 0 until w
          y <- 0 until h
          mark = (x, y)
          if !marks.contains(mark)
          if legalMove(mark, marks.head)
        yield
          //println(Seq(mark).concat(marks))
          //println(render(Seq(mark).concat(marks), w, h))
          Seq(mark).concat(marks)

    println(_placeMarks(w * h))
    _placeMarks(w * h)

  println(placeMarks(7, 5))
  //.foreach(sol => println(render(solution = sol, width = 5, height = 5)))
