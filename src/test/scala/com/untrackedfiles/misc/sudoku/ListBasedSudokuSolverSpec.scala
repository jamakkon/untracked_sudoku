package com.untrackedfiles.misc.sudoku

import org.specs2.mutable.Specification

class ListBasedSudokuSolverSpec extends Specification {

  "ListBasedSudokuSolver" should {

    "prune nothing from an empty board" in {
      val str = "................................................................................."
      val pruned = ArrayBasedSudokuSolver.prune(ArrayBasedBoard.parse(str))

      pruned.isFinished mustEqual (false)
      forall(pruned.cellIterator)((_: Cell).cands mustEqual Set[Int](1, 2, 3, 4, 5, 6, 7, 8, 9))
    }

    "prune redundant candidates in an incomplete board" in {
      val str = "1................................................................................"
      val pruned = ListBasedSudokuSolver.prune(ListBasedBoard.parse(str))

      forall(pruned.cellIterator.filter(c => c.x == 0 && c.y == 0))((_: Cell).cands mustEqual Set[Int](1))
      forall(pruned.cellIterator.filter(c => c.x == 0 && c.y != 0))((_: Cell).cands mustEqual Set[Int](2, 3, 4, 5, 6, 7, 8, 9))
      forall(pruned.cellIterator.filter(c => c.x != 0 && c.y == 0))((_: Cell).cands mustEqual Set[Int](2, 3, 4, 5, 6, 7, 8, 9))
    }

    "prune redundant candidates in a complete board" in {
      val str = "123456789456789123789123456234567891567891234891234567345678912678912345912345678"
      val pruned = ListBasedSudokuSolver.prune(ListBasedBoard.parse(str))

      pruned.toString mustEqual (str)
    }

    "solve complete and finished board" in {
      val str = "123456789456789123789123456234567891567891234891234567345678912678912345912345678"
      val board = ListBasedSudokuSolver.prune(ListBasedBoard.parse(str))

      val result = ListBasedSudokuSolver.solve(board)
      result.nonEmpty mustEqual (true)
      result.get.isFinished mustEqual (true)

      result.getOrElse("").toString mustEqual (str)
    }

    "solve nearly complete and finished board" in {
      val str = ".234567894567891237891234562345678915678912.489123456734567..12678912345912345678"
      val board = ListBasedSudokuSolver.prune(ListBasedBoard.parse(str))

      val result = ListBasedSudokuSolver.solve(board)
      result.nonEmpty mustEqual (true)
      result.get.isFinished mustEqual (true)
      result.getOrElse("").toString mustEqual
        ("123456789456789123789123456234567891567891234891234567345678912678912345912345678")
    }

    "solve incomplete board" in {
      val str = "003020600900305001001806400008102900700000008006708200002609500800203009005010300"
      val board = ListBasedSudokuSolver.prune(ListBasedBoard.parse(str))


      val result = ListBasedSudokuSolver.solve(board)
      result.nonEmpty mustEqual (true)
      result.get.isFinished mustEqual (true)
      result.getOrElse("").toString mustEqual
        ("483921657967345821251876493548132976729564138136798245372689514814253769695417382")
    }

    "solve difficult board" in {
      val str = "1.....3.8.6.4..............2.3.1...........958.........5.6...7.....8.2...4......."
      val board = ListBasedSudokuSolver.prune(ListBasedBoard.parse(str))

      val result = ListBasedSudokuSolver.solve(board)
      result.nonEmpty mustEqual (true)
      result.get.isFinished mustEqual (true)
      result.getOrElse("").toString mustEqual
        ("124597368369428517587361924293815746416273895875946132958632471631784259742159683")
    }

  }

}
