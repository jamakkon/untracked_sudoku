package com.untrackedfiles.misc.sudoku

import java.io.File

import com.typesafe.scalalogging.LazyLogging

import scala.annotation.tailrec
import scala.io.Source
import scala.util.control.NonFatal

case class ListBasedBoard(cells: List[Cell]) extends SudokuBoard {

  override def cell(x: Int, y: Int): Cell = cells.filter(c => c.y == y && c.x == x).head

  override def row(y: Int): List[Cell] = cells.filter(_.y == y)

  override def col(x: Int): List[Cell] = cells.filter(_.x == x)

  override def cellIterator: Iterator[Cell] = cells.iterator

  override def block(x: Int, y: Int): List[Cell] =
    cells.filter(c => coords(y).contains(c.y) && coords(x).contains(c.x))

  override def neighborhood(x: Int, y: Int): List[Cell] =
    (row(y) ::: col(x) ::: block(x, y)).filterNot(c => c.y == y && c.x == x)

  override def isValid: Boolean = isCellValuesOk && hasNoDuplicates

  override def isFinished: Boolean = unassigned.isEmpty && isValid

  override def toString: String = cells.map(_.value.getOrElse(".")).mkString("")

  def +(c: Cell): ListBasedBoard =
    ListBasedBoard((cells.filterNot(d => d.x == c.x && d.y == c.y) :+ c).sortBy(d => (d.y, d.x)))

  def hasNoDuplicates: Boolean = {
    cells.filter(_.value.nonEmpty).map { cell =>
      cell.value.map { v =>
        val neighborhoodValues: Set[Int] = neighborhood(cell.x, cell.y).flatMap(_.value).toSet

        !neighborhoodValues.contains(v)
      }.getOrElse(true)
    }.filter(_.equals(false)).isEmpty
  }

  def coords(z: Int): Set[Int] = {
    if (0 <= z && z < 3) {
      Set[Int](0, 1, 2)
    } else if (3 <= z && z < 6) {
      Set[Int](3, 4, 5)
    } else if (6 <= z && z < 9) {
      Set[Int](6, 7, 8)
    } else {
      Set.empty[Int]
    }
  }

}


object ListBasedBoard {

  val DigitPattern = """^\d$""".r

  def parse(str: String): ListBasedBoard = {
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

    val cells = for {
      x <- 0 until 9
      y <- 0 until 9
    } yield {
      if (cands(y * 9 + x) > 0)
        Cell(x, y, Set[Int](cands(y * 9 + x)))
      else
        Cell(x, y, Set[Int](1, 2, 3, 4, 5, 6, 7, 8, 9))
    }
    assert(cells.length == 81, s"Invalid number of cells (${cells.length} != 81).")

    ListBasedBoard(cells.sortBy(c => (c.y, c.x)).toList)
  }

}

object ListBasedSudokuSolver extends LazyLogging {

  def solve(board: ListBasedBoard): Option[ListBasedBoard] = {
    val stamp: Long = System.currentTimeMillis()
    val result = iterate(board)
    val elapsed: Long = System.currentTimeMillis() - stamp

    logger.info(s"Finished in ${elapsed} ms (board: $result)")
    result
  }

  def iterate(board: ListBasedBoard): Option[ListBasedBoard] = {
    logger.trace(s"iterate: $board\n${board.prettyString}")

    if (!board.isValid) {
      None
    } else if (board.isFinished) {
      Some(board)
    } else {
      val unassignedCells = prune(board).cells.filter(_.value.isEmpty).sortBy(_.cands.size)

      unassignedCells.map { cell =>
        cell.cands.map { c =>
          val result = iterate(prune(board + Cell(cell.x, cell.y, Set[Int](c))))

          if (result.nonEmpty && result.get.isFinished) {
            return result
          }
        }
        return None
      }
      None
    }
  }


  @tailrec
  def prune(board: ListBasedBoard): ListBasedBoard = {
    val candsBefore = board.cells.count(_.value.nonEmpty)
    val prunedCells: List[Cell] = board.cells.map { cell =>
      val prunedCandidates = if (cell.value.nonEmpty) {
        cell.cands
      } else {
        cell.cands.diff(board.neighborhood(cell.x, cell.y).flatMap(_.value).toSet)
      }
      Cell(cell.x, cell.y, prunedCandidates)
    }
    val candsAfter = prunedCells.count(_.value.nonEmpty)
    val emptyCandidates = prunedCells.count(_.cands.isEmpty)

    if (candsBefore < candsAfter && emptyCandidates == 0) {
      prune(ListBasedBoard(prunedCells))
    } else {
      ListBasedBoard(prunedCells)
    }
  }
}

object ListBasedSudokuBenchmark extends LazyLogging {

  def main(args: Array[String]): Unit = {
    val inputFilename = args(0)
    val data: List[String] = readSudokuFile(inputFilename)

    val batch = data.par.map { sudokuString =>
      val board = ListBasedBoard.parse(sudokuString)
      val start: Long = System.currentTimeMillis
      val result = ListBasedSudokuSolver.solve(board)
      val elapsed = System.currentTimeMillis - start

      (elapsed, result.nonEmpty)
    }
    val avgTimeAll = batch.map(_._1).sum / batch.size.toDouble
    val solvedCount = batch.count(_._2)

    println(s"Sample file: $inputFilename")
    println(s"Sample size: ${data.size}")
    println(s"Solved samples: ${solvedCount}")
    println(s"Average time: ${avgTimeAll}")
  }

  def readSudokuFile(filename: String): List[String] = {
    val src = Source.fromFile(new File(filename))
    try {
      src.getLines.map(_.trim).filter(_.nonEmpty).toList
    } catch {
      case NonFatal(e) =>
        logger.error(s"Error: $e", e.fillInStackTrace())
        Nil
    } finally {
      src.close
    }
  }
}