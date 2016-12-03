package com.untrackedfiles.misc.sudoku

import java.io.File

import com.typesafe.scalalogging.LazyLogging

import scala.annotation.tailrec
import scala.io.Source
import scala.util.control.NonFatal


case class ArrayBasedBoard(grid: Array[Array[Cell]]) extends SudokuBoard {

  override def cell(x: Int, y: Int): Cell = grid(y)(x)

  override def row(y: Int): List[Cell] = grid(y).toList

  override def col(x: Int): List[Cell] = grid.map(r => r(x)).toList

  override def block(x: Int, y: Int): List[Cell] = {
    val xmin = SudokuBoard.toBlockCoord(x)
    val ymin = SudokuBoard.toBlockCoord(y)

    grid.slice(ymin, ymin + 3).flatMap(r => r.slice(xmin, xmin + 3)).toList
  }

  override def neighborhood(x: Int, y: Int): List[Cell] = (row(y) ++ col(x) ++ block(x, y))

  override def cellIterator: Iterator[Cell] = grid.flatMap(r => r.iterator).iterator

  override def isFinished: Boolean = grid.flatMap(r => r.filterNot(_.value.nonEmpty)).isEmpty

  override def isValid: Boolean = isCellValuesOk && isConsistent

  def isConsistent: Boolean = {
    val rowsOk = (0 until 9).collectFirst {
      case y if SudokuBoard.hasDuplicateValues(row(y)) => false
    }.getOrElse(true)

    if (rowsOk) {
      val columnsOk = (0 until 9).collectFirst {
        case x if SudokuBoard.hasDuplicateValues(col(x)) => false
      }.getOrElse(true)

      if (columnsOk) {
        SudokuBoard.CoordPairs.filter(c => (c._1 % 3 == 0 && c._2 % 3 == 0)).
          collectFirst {
            case (x, y) if SudokuBoard.hasDuplicateValues(block(x, y)) => false
          }.getOrElse(true)
      } else {
        false
      }
    } else {
      false
    }
  }

  def +(c: Cell): ArrayBasedBoard = {
    val newGrid = gridCopy
    newGrid(c.y)(c.x) = c

    ArrayBasedBoard(newGrid)
  }

  private def gridCopy: Array[Array[Cell]] = {
    grid.map { row =>
      val newArray = new Array[Cell](row.size)

      row.copyToArray(newArray)
      newArray
    }
  }

  override def toString: String = cellIterator.map(c => c.value.getOrElse(".")).mkString("")

}


object ArrayBasedBoard {

  def parse(str: String): ArrayBasedBoard = {
    val cands: List[Int] = str.split("").flatMap { c =>
      if (SudokuBoard.DigitPattern.findFirstIn(c).nonEmpty) {
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

}

object ArrayBasedSudokuSolver extends LazyLogging {

  def solve(board: ArrayBasedBoard): Option[ArrayBasedBoard] = {

    val stamp: Long = System.currentTimeMillis()
    val result = iterateUnassigned(board)
    val elapsed: Long = System.currentTimeMillis() - stamp

    if (result.nonEmpty)
      logger.info(s"Solved in ${elapsed} ms (board: $result)")
    else
      logger.info(s"Finished in ${elapsed} ms (board: $result)")
    result
  }

  def iterateUnassigned(board: ArrayBasedBoard): Option[ArrayBasedBoard] = {

    board match {
      case board if !board.isValid =>
        None

      case board if board.isFinished =>
        Some(board)

      case board =>
        board.unassigned.toList match {
          case Nil =>
            Some(board)

          case head :: tail =>
            head.cands.map { candidate =>
              val newBoard = prune(board + Cell(head.x, head.y, Set[Int](candidate)))
              val result = iterateUnassigned(newBoard)

              if (result.nonEmpty && result.get.isValid && result.get.isFinished)
                return result
            }
            None
        }
    }
  }


  @tailrec
  final def prune(board: ArrayBasedBoard, changes: Int = 1): ArrayBasedBoard = {

    if (changes == 0) {
      board
    } else if (board.isFinished && board.isValid) {
      board
    } else {
      var changes = 0
      val grid = (0 until 9).map { y =>
        (0 until 9).map { x =>
          val cell = board.cell(x, y)

          if (cell.value.nonEmpty) {
            cell
          } else {
            val nbHood = board.neighborhood(cell.x, cell.y).filterNot(c => c.x == x && c.y == y).
              filter(_.value.nonEmpty).flatMap(_.value).toSet

            if (nbHood.nonEmpty && cell.cands.diff(nbHood).nonEmpty && cell.cands.diff(nbHood) != cell.cands) {
              changes += 1
              Cell(cell.x, cell.y, cell.cands.diff(nbHood))
            } else {
              cell
            }
          }
        }.toArray
      }.toArray
      val newBoard = ArrayBasedBoard(grid)

      if (changes == 0) {
        // Eliminate twice occurring candidate pairs from rows.
        (0 until 9).foreach { y =>
          eliminatePairs(newBoard.row(y)).foreach { cell =>
            newBoard.grid(cell.y)(cell.x) = cell
            changes += 1
          }
        }

        // Eliminate twice occurring candidate pairs from columns.
        (0 until 9).foreach { y =>
          eliminatePairs(newBoard.col(y)).foreach { cell =>
            newBoard.grid(cell.y)(cell.x) = cell
            changes += 1
          }
        }

        // Eliminate twice occurring candidate pairs from blocks.
        SudokuBoard.CoordPairs.filter(c => (c._1 % 3 == 0 && c._2 % 3 == 0)).foreach { coord =>
          eliminatePairs(newBoard.block(coord._1, coord._2)).foreach { cell =>
            newBoard.grid(cell.y)(cell.x) = cell
            changes += 1
          }
        }
      }

      if (changes > 0)
        prune(newBoard, changes)
      else
        board
    }
  }

  private def eliminatePairs(cells: Traversable[Cell]): List[Cell] = {
    twiceOccurringPairs(cells).flatMap { pair =>
      cells.collect {
        case cell if (cell.value.isEmpty && cell.cands != pair && cell.cands.diff(pair) != cell.cands) =>
          Cell(cell.x, cell.y, cell.cands.diff(pair))
      }
    }.toList
  }

  private def twiceOccurringPairs(cells: Traversable[Cell]): Set[Set[Int]] = {
    cells.filter(_.cands.size == 2).groupBy(_.cands).map(x => x._1 -> x._2.size).
      filter(_._2 == 2).map(_._1).toSet
  }

}

object ArrayBasedSudokuBenchmark extends LazyLogging {

  def main(args: Array[String]): Unit = {
    val inputFilename = args(0)
    val data: List[String] = readSudokuFile(inputFilename)

    val batch = data.par.map { sudokuString =>
      val board = ArrayBasedBoard.parse(sudokuString)
      val start: Long = System.currentTimeMillis
      val result = ArrayBasedSudokuSolver.solve(board)
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
