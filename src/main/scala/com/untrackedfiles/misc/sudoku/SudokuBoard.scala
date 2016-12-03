package com.untrackedfiles.misc.sudoku

case class Cell(x: Int, y: Int, cands: Set[Int]) {

  def value: Option[Int] = {
    if (cands.size == 1) {
      cands.headOption
    } else {
      None
    }
  }

  override def toString: String = s"Cell(x=$x, y=$y, cands=$cands)"

}

trait SudokuBoard {

  def cell(x: Int, y: Int): Cell

  def row(y: Int): List[Cell]

  def col(x: Int): List[Cell]

  def block(x: Int, y: Int): List[Cell]

  def neighborhood(x: Int, y: Int): List[Cell]

  def cellIterator: Iterator[Cell]

  def unassigned: Iterator[Cell] = cellIterator.filter(_.value.isEmpty)

  def isFinished: Boolean

  def isValid: Boolean

  def isCellValuesOk: Boolean = {
    cellIterator.collect {
      case (cell) if (cell.cands.isEmpty) => cell
    }.isEmpty
  }

  def prettyString: String = {
    val maxCands = cellIterator.map(_.cands.size).max
    val pipes = Set[Int](2, 5, 8)
    val line = "+" + (0 until 9).map(x => "-" * maxCands + { if (pipes(x)) "-+" else "-" }).mkString("-")

    s"\n$line\n" + (0 until 9).map { y =>
      "|" + (0 until 9).map { x =>
          (String.format(s"%${maxCands}s", cell(x, y).cands.toList.sorted.mkString(""))
            + { if (pipes(x)) " |" else " " })
        }.mkString(" ") + {
        if (pipes(y)) s"\n$line" else ""
      }
    }.mkString("\n") + s"\n"
  }

}

object SudokuBoard {

  /** Check if given row, column, or block contains the same value more than once. */
  def hasDuplicateValues(cells: Traversable[Cell]): Boolean = {
    cells.filter(_.value.nonEmpty).
      flatMap(_.value).
      groupBy(v => v).
      map(x => x._2.size).
      filter(v => v > 1).nonEmpty
  }

  val DigitPattern = """^\d$""".r

  val CoordPairs: List[(Int, Int)] =
    (0 until 9).map(p => (0 until 9).map(o => (p, o))).flatten.toList

  def toBlockCoord(z: Int): Int =
    z match {
      case z if (0 <= z && z < 3) => 0
      case z if (3 <= z && z < 6) => 3
      case z if (6 <= z && z < 9) => 6
      case _ => -1
    }

}