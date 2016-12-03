package com.untrackedfiles.misc.sudoku

import org.specs2.mutable.Specification

class SudokuBoardSpec extends Specification {

  "hasDuplicateValues" should {

    def test(cells: Traversable[Cell], expected: Boolean) = {
      SudokuBoard.hasDuplicateValues(cells) mustEqual (expected)
    }

    "recognize duplicates" in {
      List(
        (List(Cell(0, 0, Set[Int](1, 2, 3, 4))), false),
        (List(Cell(0, 0, Set[Int](1, 2, 3, 4)), Cell(1, 0, Set[Int](1, 2, 3, 4))), false),
        (List(Cell(0, 0, Set[Int](1))), false),
        (List(Cell(0, 0, Set[Int](1)), Cell(1, 0, Set[Int](2))), false),
        (List(Cell(0, 0, Set[Int](1)), Cell(1, 0, Set[Int](1))), true)
      ).map(p => test(p._1, p._2))
    }

  }

}
