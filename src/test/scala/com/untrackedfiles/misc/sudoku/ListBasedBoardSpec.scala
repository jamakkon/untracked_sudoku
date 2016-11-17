package com.untrackedfiles.misc.sudoku

import org.specs2.mutable._

class ListBasedBoardSpec extends Specification {


  /**
    * +---+
    * |123|
    * |456|
    * |789|
    * +---+
    */
  val cells: List[Cell] = List[Cell](
    Cell(0, 0, Set[Int](1)),
    Cell(1, 0, Set[Int](2)),
    Cell(2, 0, Set[Int](3)),
    Cell(0, 1, Set[Int](4)),
    Cell(1, 1, Set[Int](5)),
    Cell(2, 1, Set[Int](6)),
    Cell(0, 2, Set[Int](7)),
    Cell(1, 2, Set[Int](8)),
    Cell(2, 2, Set[Int](9))
  )

  val miniBoard: ListBasedBoard = ListBasedBoard(cells)

  "ListBasedBoard.parse" should {

    "parse complete board from string" in {
      val str = "123456789456789123789123456234567891567891234891234567345678912678912345912345678"
      val board = ListBasedBoard.parse(str)

      board.cells.filter(_.value.isEmpty).isEmpty mustEqual (true)
      board.toString mustEqual (str)
    }

    "parse empty board from string" in {
      val exp = "." * 81
      val str = "0" * 81
      val board = ListBasedBoard.parse(str)

      board.cells.filter(_.value.isDefined).isEmpty mustEqual (true)
      board.toString mustEqual (exp)
    }
  }

  "cell" should {

    "return given cell" in {
      miniBoard.cell(0, 0)(0).value.getOrElse(0) mustEqual (1)
      miniBoard.cell(1, 0)(0).value.getOrElse(0) mustEqual (2)
      miniBoard.cell(2, 0)(0).value.getOrElse(0) mustEqual (3)

      miniBoard.cell(0, 1)(0).value.getOrElse(0) mustEqual (4)
      miniBoard.cell(1, 1)(0).value.getOrElse(0) mustEqual (5)
      miniBoard.cell(2, 1)(0).value.getOrElse(0) mustEqual (6)

      miniBoard.cell(0, 2)(0).value.getOrElse(0) mustEqual (7)
      miniBoard.cell(1, 2)(0).value.getOrElse(0) mustEqual (8)
      miniBoard.cell(2, 2)(0).value.getOrElse(0) mustEqual (9)
    }
  }

  "row" should {

    "return given row" in {
      miniBoard.row(0)(0).value.getOrElse(0) mustEqual (1)
      miniBoard.row(0)(1).value.getOrElse(0) mustEqual (2)
      miniBoard.row(0)(2).value.getOrElse(0) mustEqual (3)

      miniBoard.row(1)(0).value.getOrElse(0) mustEqual (4)
      miniBoard.row(1)(1).value.getOrElse(0) mustEqual (5)
      miniBoard.row(1)(2).value.getOrElse(0) mustEqual (6)

      miniBoard.row(2)(0).value.getOrElse(0) mustEqual (7)
      miniBoard.row(2)(1).value.getOrElse(0) mustEqual (8)
      miniBoard.row(2)(2).value.getOrElse(0) mustEqual (9)
    }
  }

  "column" should {

    "return given column" in {
      miniBoard.col(0)(0).value.getOrElse(0) mustEqual (1)
      miniBoard.col(0)(1).value.getOrElse(0) mustEqual (4)
      miniBoard.col(0)(2).value.getOrElse(0) mustEqual (7)

      miniBoard.col(1)(0).value.getOrElse(0) mustEqual (2)
      miniBoard.col(1)(1).value.getOrElse(0) mustEqual (5)
      miniBoard.col(1)(2).value.getOrElse(0) mustEqual (8)

      miniBoard.col(2)(0).value.getOrElse(0) mustEqual (3)
      miniBoard.col(2)(1).value.getOrElse(0) mustEqual (6)
      miniBoard.col(2)(2).value.getOrElse(0) mustEqual (9)
    }
  }

  "block" should {

    def test(board: ListBasedBoard, xy: (Int, Int), blockXY: (Int, Int)) = {
      board.block(xy._1, xy._2) mustEqual (board.block(blockXY._1, blockXY._2))
    }

    /**
      * +-----------------------+
      * | 1 2 3 | 4 5 6 | 7 8 9 |
      * | 4 5 6 | 7 8 9 | 1 2 3 |
      * | 7 8 9 | 1 2 3 | 4 5 6 |
      * |-------+-------+-------|
      * | 2 3 4 | 5 6 7 | 8 9 1 |
      * | 5 6 7 | 8 9 1 | 2 3 4 |
      * | 8 9 1 | 2 3 4 | 5 6 7 |
      * |-------+-------+-------|
      * | 3 4 5 | 6 7 8 | 9 1 2 |
      * | 6 7 8 | 9 1 2 | 3 4 5 |
      * | 9 1 2 | 3 4 5 | 6 7 8 |
      * +-----------------------+
      */
    "return associated box in complete board" in {
      val str = "123456789456789123789123456234567891567891234891234567345678912678912345912345678"
      val board = ListBasedBoard.parse(str)

      List(
        // upper left block
        ((1, 0), (0, 0)),
        ((2, 0), (0, 0)),
        ((0, 1), (0, 0)),
        ((1, 1), (0, 0)),
        ((2, 1), (0, 0)),
        ((0, 2), (0, 0)),
        ((1, 2), (0, 0)),
        ((2, 2), (0, 0)),
        // upper middle block
        ((4, 0), (3, 0)),
        ((5, 0), (3, 0)),
        ((3, 1), (3, 0)),
        ((4, 1), (3, 0)),
        ((5, 1), (3, 0)),
        ((3, 2), (3, 0)),
        ((4, 2), (3, 0)),
        ((5, 2), (3, 0)),
        // upper right block
        ((7, 0), (6, 0)),
        ((8, 0), (6, 0)),
        ((6, 1), (6, 0)),
        ((7, 1), (6, 0)),
        ((8, 1), (6, 0)),
        ((6, 2), (6, 0)),
        ((7, 2), (6, 0)),
        ((8, 2), (6, 0)),
        // upper right block
        ((7, 0), (6, 0)),
        ((8, 0), (6, 0)),
        ((6, 1), (6, 0)),
        ((7, 1), (6, 0)),
        ((8, 1), (6, 0)),
        ((6, 2), (6, 0)),
        ((7, 2), (6, 0)),
        ((8, 2), (6, 0)),
        // middle left block
        ((1, 3), (0, 3)),
        ((2, 3), (0, 3)),
        ((0, 4), (0, 3)),
        ((1, 4), (0, 3)),
        ((2, 4), (0, 3)),
        ((0, 5), (0, 3)),
        ((1, 5), (0, 3)),
        ((2, 5), (0, 3)),
        // middle middle block
        ((4, 3), (3, 3)),
        ((5, 3), (3, 3)),
        ((3, 4), (3, 3)),
        ((4, 4), (3, 3)),
        ((5, 4), (3, 3)),
        ((3, 5), (3, 3)),
        ((4, 5), (3, 3)),
        ((5, 5), (3, 3)),
        // middle right block
        ((7, 3), (6, 3)),
        ((8, 3), (6, 3)),
        ((6, 4), (6, 3)),
        ((7, 4), (6, 3)),
        ((8, 4), (6, 3)),
        ((6, 5), (6, 3)),
        ((7, 5), (6, 3)),
        ((8, 5), (6, 3)),
        // lower left block
        ((1, 6), (0, 6)),
        ((2, 6), (0, 6)),
        ((0, 7), (0, 6)),
        ((1, 7), (0, 6)),
        ((2, 7), (0, 6)),
        ((0, 8), (0, 6)),
        ((1, 8), (0, 6)),
        ((2, 8), (0, 6)),
        // lower middle block
        ((4, 6), (3, 6)),
        ((5, 6), (3, 6)),
        ((3, 7), (3, 6)),
        ((4, 7), (3, 6)),
        ((5, 7), (3, 6)),
        ((3, 8), (3, 6)),
        ((4, 8), (3, 6)),
        ((5, 8), (3, 6)),
        // lower right block
        ((7, 6), (6, 6)),
        ((8, 6), (6, 6)),
        ((6, 7), (6, 6)),
        ((7, 7), (6, 6)),
        ((8, 7), (6, 6)),
        ((6, 8), (6, 6)),
        ((7, 8), (6, 6)),
        ((8, 8), (6, 6))
      ).map(p => test(board, p._1, p._2))
    }
  }

  "neighborhood" should {
    /**
      * +-----------------------+
      * | 1 2 3 | 4 5 6 | 7 8 9 |
      * | 4 5 6 | 7 8 9 | 1 2 3 |
      * | 7 8 9 | 1 2 3 | 4 5 6 |
      * |-------+-------+-------|
      * | 2 3 4 | 5 6 7 | 8 9 1 |
      * | 5 6 7 | 8 9 1 | 2 3 4 |
      * | 8 9 1 | 2 3 4 | 5 6 7 |
      * |-------+-------+-------|
      * | 3 4 5 | 6 7 8 | 9 1 2 |
      * | 6 7 8 | 9 1 2 | 3 4 5 |
      * | 9 1 2 | 3 4 5 | 6 7 8 |
      * +-----------------------+
      */
    "return associated neighborhood in a complete board" in {
      val str = "123456789456789123789123456234567891567891234891234567345678912678912345912345678"
      val board = ListBasedBoard.parse(str)

      val nbValues1: Set[Int] = board.neighborhood(3, 0).flatMap(_.value)
      nbValues1.size mustEqual (8)
      nbValues1.contains(4) mustEqual (false)

      val nbValues2: Set[Int] = board.neighborhood(6, 7).flatMap(_.value)
      nbValues2.size mustEqual (8)
      nbValues2.contains(3) mustEqual (false)
    }

    /**
      * +---+---+---+
      * |85.|..2|4..|
      * |72.|...|..9|
      * |..4|...|...|
      * +---+---+---+
      * |...|1.7|..2|
      * |3.5|...|9..|
      * |.4.|...|...|
      * +---+---+---+
      * |...|.8.|.7.|
      * |.17|...|...|
      * |...|.36|.4.|
      * +---+---+---+
      */
    "return associated neighborhood in an incomplete board" in {
      val str = "85...24..72......9..4.........1.7..23.5...9...4...........8..7..17..........36.4."
      val board = ListBasedBoard.parse(str)

      board.neighborhood(0, 0).flatMap(_.value) mustEqual (Set[Int](7, 5, 2, 4, 3))
      board.neighborhood(1, 5).flatMap(_.value) mustEqual (Set[Int](1, 3, 5, 2))
    }
  }

  "isFinished" should {

    "recognize complete board as finished" in {
      val str = "123456789456789123789123456234567891567891234891234567345678912678912345912345678"
      val board = ListBasedBoard.parse(str)

      board.isFinished mustEqual (true)
    }

    "recognize incomplete board as not finished" in {
      val str = "12345678945678912378912345623.567891567891234891234567345678912678912345912345678"
      val board = ListBasedBoard.parse(str)

      board.isFinished mustEqual (false)
    }

  }

  "isCellValuesOk" should {

    "recognize cells of an empty board as valid" in {
      val str = "." * 81
      val board = ListBasedBoard.parse(str)

      board.isCellValuesOk mustEqual (true)
    }

    "recognize cells of a complete board as valid" in {
      val str = "123456789456789123789123456234567891567891234891234567345678912678912345912345678"
      val board = ListBasedBoard.parse(str)

      board.isCellValuesOk mustEqual (true)
    }

    "recognize an invalid cell" in {
      val str = "123456789456789123789123456234567891567891234891234567345678912678912345912345678"
      val board = ListBasedBoard.parse(str) + Cell(0, 0, Set.empty[Int])

      board.isCellValuesOk mustEqual (false)
    }

  }


  "isValid" should {

    "recognize empty board as valid" in {
      val str = "." * 81
      val board = ListBasedBoard.parse(str)

      board.isValid mustEqual (true)
    }

    "recognize valid and complete board as valid" in {
      val str = "123456789456789123789123456234567891567891234891234567345678912678912345912345678"
      val board = ListBasedBoard.parse(str)

      board.isValid mustEqual (true)
    }

    "recognize invalid board as invalid" in {
      val str = "113456789456789123789123456234567891567891234891234567345678912678912345912345678"
      val board = ListBasedBoard.parse(str)

      board.isValid mustEqual (false)
    }
  }


  "add" should {

    "add cell to an empty board" in {
      val str = "." * 81
      val board = ListBasedSudokuSolver.prune(ListBasedBoard.parse(str) +
        Cell(0, 0, Set[Int](9)) +
        Cell(4, 2, Set[Int](3, 5, 1)))

      board.cells.length mustEqual (81)
      board.cell(0, 0)(0).value.getOrElse(0) mustEqual (9)
      board.cell(0, 0)(0).cands mustEqual (Set[Int](9))
      board.neighborhood(0, 0).filter(_.value.isEmpty).flatMap(_.cands).contains(9) mustEqual (false)

      board.cell(4, 2)(0).value mustEqual (None)
      board.cell(4, 2)(0).cands mustEqual (Set[Int](1, 3, 5))
      board.neighborhood(4, 2).filter(_.value.isEmpty).flatMap(_.cands).contains(5) mustEqual (true)
    }

  }

}