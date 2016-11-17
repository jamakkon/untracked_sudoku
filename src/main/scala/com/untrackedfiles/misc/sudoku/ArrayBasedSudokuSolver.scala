package com.untrackedfiles.misc.sudoku


case class ArrayBasedBoard(grid: Array[Array[Cell]]) {

  def row(y: Int): Array[Cell] = grid(y)

  def col(x: Int): Array[Cell] = grid.map(r => r(x))

  def cell(x: Int, y: Int): Cell = grid(y)(x)

  def block(x: Int, y: Int): Array[Cell] = {
    val ymin = ArrayBasedBoard.coordMin(y)
    val xmin = ArrayBasedBoard.coordMin(x)

    grid.slice(ymin, ymin + 2).flatMap(r => r.slice(xmin, xmin + 2))
  }

  def neighborhood(x: Int, y: Int): Array[Cell] = row(y) ++ col(x) ++ block(x, y)

  def cells: Iterator[Cell] = grid.flatMap(r => r.iterator).iterator

  def isFinished: Boolean = grid.flatMap(r => r.filterNot(_.value.nonEmpty)).isEmpty

  def isValid: Boolean = cellValuesOk && isConsistent

  def cellValuesOk: Boolean = grid.flatMap(r => r.filterNot(c => c.cands.size > 0)).isEmpty

  def isConsistent: Boolean = {
    ArrayBasedBoard.CoordPairs.map { z =>
      neighborhood(z._1, z._2).
        filter(_.value.nonEmpty).
        flatMap(_.value).
        groupBy(r => r).
        map(x => x._1 -> x._2.size).
        map(x => x._2).
        filter(count => count > 1).
        isEmpty
    }.filter(_ == false).isEmpty
  }

  override def toString: String = cells.map(c => c.value.getOrElse(".")).mkString("")
}


object ArrayBasedBoard {

  val DigitPattern = """^\d$""".r

  val CoordPairs: List[(Int, Int)] =
    (0 until 9).map(p => (0 until 9).map(o => (p, o))).flatten.toList

  def coordMin(z: Int): Int =
    z match {
      case z if (0 <= z && z < 3) => 0
      case z if (3 <= z && z < 6) => 3
      case z if (6 <= z && z < 9) => 6
      case _ => -1
    }

  def parse(str: String): ArrayBasedBoard = {
    val cands: List[Int] = str.split("").flatMap { c =>
      if (DigitPattern.findFirstIn(c).nonEmpty) {
        Some(c.toInt)
      } else if (c.equals(".")) {
        Some(0)
      } else {
        None
      }
    }.toList
    assert(str.length == 81, s"Invalid length of input (${cands.length} != 81).")

    val grid = (0 until 9).map { y =>
      (0 until 9).map { x =>
        if (cands(y * 9 + x) > 0)
          Cell(x, y, Set[Int](cands(y * 9 + x)))
        else
          Cell(x, y, Set[Int](1, 2, 3, 4, 5, 6, 7, 8, 9))
      }.toArray
    }.toArray

    ArrayBasedBoard(grid)
  }

  def prettyString(board: ArrayBasedBoard): String = {
    val maxCands = board.cells.map(_.cands.size).max
    val pipes = Set[Int](2, 5, 8)
    val line = "+" + (0 until 9).map(x => "-" * maxCands + {
      if (pipes(x)) "-+" else "-"
    }).mkString("-")

    s"\n$line\n" + (0 until 9).map { y =>
      "|" +
        (0 until 9).map { x =>
          (s"%${maxCands}s", board.cell(x, y).cands.toList.sorted.mkString("")
            + {
            if (pipes(x)) " |" else " "
          })
        }.mkString(" ") + {
        if (pipes(y)) s"\n$line" else ""
      }
    }.mkString("\n") + s"\n"
  }
}

class ArrayBasedSudokuSolver {

}
