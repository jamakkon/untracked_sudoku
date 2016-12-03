package com.untrackedfiles.misc.sudoku

import org.specs2.mutable.Specification

class ArrayBasedSudokuSolverSpec extends Specification {

  "ArrayBasedSudokuSolver" should {
/*
    "prune nothing from an empty board" in {
      val str = "................................................................................."
      val pruned = ArrayBasedSudokuSolver.prune(ArrayBasedBoard.parse(str))

      pruned.isFinished mustEqual (false)
      forall(pruned.cellIterator)((_: Cell).cands mustEqual Set[Int](1, 2, 3, 4, 5, 6, 7, 8, 9))
    }

    "prune redundant candidates" in {
      val str = "1................................................................................"
      val pruned = ArrayBasedSudokuSolver.prune(ArrayBasedBoard.parse(str))

      forall(pruned.cellIterator.filter(c => c.x == 0 && c.y == 0))((_: Cell).cands mustEqual Set[Int](1))
      forall(pruned.cellIterator.filter(c => c.x == 0 && c.y != 0))((_: Cell).cands mustEqual Set[Int](2, 3, 4, 5, 6, 7, 8, 9))
      forall(pruned.cellIterator.filter(c => c.x != 0 && c.y == 0))((_: Cell).cands mustEqual Set[Int](2, 3, 4, 5, 6, 7, 8, 9))
    }

    "solve complete and finished puzzle" in {
      val str = "123456789456789123789123456234567891567891234891234567345678912678912345912345678"
      val board = ArrayBasedBoard.parse(str)

      val result = ArrayBasedSudokuSolver.solve(board)
      result.nonEmpty mustEqual (true)
      result.get.isValid mustEqual (true)
      result.get.isFinished mustEqual (true)

      result.getOrElse("").toString mustEqual (str)
    }

    "solve nearly complete and finished puzzle" in {
      val str = ".234567894567891237891234562345678915678912.489123456734567..12678912345912345678"
      val board = ArrayBasedBoard.parse(str)

      val result = ArrayBasedSudokuSolver.solve(board)
      result.nonEmpty mustEqual (true)
      result.get.isFinished mustEqual (true)
      result.getOrElse("").toString mustEqual
        ("123456789456789123789123456234567891567891234891234567345678912678912345912345678")
    }
    "solve incomplete puzzle with just pruning" in {
      val str = "5..9.12....1..759.....8.3616....891.9.......6.586....2215.4.....875..4....93.2..5"
      val board = ArrayBasedSudokuSolver.prune(ArrayBasedBoard.parse(str))

      board.isValid mustEqual (true)
      board.isFinished mustEqual (true)
      board.toString mustEqual
        ("546931278831267594792485361624758913973124856158693742215849637387516429469372185")
    }

    "solve incomplete easy puzzle" in {
      val str = "003020600900305001001806400008102900700000008006708200002609500800203009005010300"
      val board = ArrayBasedSudokuSolver.prune(ArrayBasedBoard.parse(str))

      val result = ArrayBasedSudokuSolver.solve(board)
      result.nonEmpty mustEqual (true)
      result.get.isFinished mustEqual (true)
      result.getOrElse("").toString mustEqual
        ("483921657967345821251876493548132976729564138136798245372689514814253769695417382")
    }
*/

    "solve incomplete hard puzzle" in {
      val str = ".........4.6.7..9..5..382.........3.9..........426.....7...3..2..16..8...85...7.."
      val board = ArrayBasedSudokuSolver.prune(ArrayBasedBoard.parse(str))

      val result = ArrayBasedSudokuSolver.solve(board)
      result.nonEmpty mustEqual (true)
      result.get.isFinished mustEqual (true)
      result.getOrElse("").toString mustEqual
        ("893426175426571398157938246512789634968314527734265981679853412241697853385142769")
    }

  }
}
